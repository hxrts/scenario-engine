-- | The Pacific composition's golden scenarios (docs/05_pacific.md §8): free trade, each ban, each investment,
-- | a ban met by substitution, no phantom trade, and the kernel invariants across them. Expected values are
-- | literals from docs/05, never computed.
module Test.Handlers.Pacific (spec, pacificParts) where

import Prelude

import Adapters.Filesystem (fileOps)
import Assembly.Assemble (assemble, liveBindings, withRounds)
import Assembly.Types (SeatBinding(..))
import Handlers.Seats (proposeAnswer, seatEnv)
import Laws.Engine.Prepare (observe)
import Assembly.Bindings (strategyRulesBindings)
import Assembly.Composition (CompositionParts, Origin(..))
import Checks.Run (checkRun)
import Core.Draw (Seed(..))
import Core.Num (Q)
import Core.Num as Q
import Data.Array (filter, foldl, index, length, take, zip)
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Control.Monad.Error.Class (throwError)
import Effect.Aff (Aff)
import Effect.Exception (error)
import Effect.Class (liftEffect)
import Foreign.Object as Object
import Handlers.Replay (runReplay)
import Handlers.Simulate (SimRun)
import Laws.Invariants (orderIndependent)
import Laws.Query (lookupQ)
import Laws.Slice (event, exchangeFills, levies, select, shortfalls)
import Laws.StrategyRules (Clause, StrategyRules)
import Laws.Types (ActorId, Direction(..), FlowEffect(..), FlowGovernance(..), Id(..), IntentF(..), LevyBasis(..), LevyRecipient(..), ResourceId, RuleChangeF(..), RuleF(..), RuleForm(..))
import Laws.World (RejectReason(..), RoundResult)
import Premises.Load (loadFrom, parts)
import Premises.Pacific as Pacific
import Premises.Pacific.Strategies (banRule, presetId)
import Premises.Pacific.Names (p, pacificId, pacificVersion)
import Schema.Build.Decision (asTemplate, eqQ, joint, lit, roundQ, rule, selfActor, selfTransform, signAll)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Testkit.Meridian (orThrow, run)

pacificParts :: Aff CompositionParts
pacificParts = do
  loaded <- liftEffect (loadFrom fileOps (pacificId <> "/" <> pacificVersion))
  orThrow loaded

preset :: CompositionParts -> String -> Aff StrategyRules
preset parts name = case Object.lookup name parts.presets of
  Just s -> pure s
  Nothing -> throwError (error ("no preset " <> name))

-- | Play `rounds` rounds with each country on its StrategyRules; every round's invariants must hold.
playRules :: { us :: StrategyRules, cn :: StrategyRules } -> Int -> Aff SimRun
playRules s rounds = do
  parts <- pacificParts
  g <- orThrow (assemble (FromComposition parts) (strategyRulesBindings [ Tuple p.us s.us, Tuple p.cn s.cn ]) [])
  r <- run (Seed 1) (withRounds (Just rounds) g)
  checkRun r.manifest.initial r.records `shouldEqual` []
  pure r

-- | Play with each country on a named preset.
play :: { us :: String, cn :: String } -> Int -> Aff SimRun
play names rounds = do
  parts <- pacificParts
  us <- preset parts names.us
  cn <- preset parts names.cn
  playRules { us, cn } rounds

-- | One investment in round 1; commerce is the routine beside it.
investOnce :: Array Clause
investOnce = [ rule [ eqQ roundQ (lit 1) ] [ asTemplate (selfTransform { transformation: p.usIndustrialInvestment, quantity: lit 1 }), asTemplate (selfTransform { transformation: p.cnTechnologyInvestment, quantity: lit 1 }) ] ]

noPolicy :: StrategyRules
noPolicy = { rules: [], sign: [] }

roundOf :: SimRun -> Int -> Aff RoundResult
roundOf r k = case index r.records (k - 1) of
  Just x -> pure x
  Nothing -> throwError (error ("no round " <> show k))

holdingAt :: RoundResult -> ActorId -> ResourceId -> Q
holdingAt r a res = fromMaybe Q.zero (lookupQ res <<< _.accounts <$> Map.lookup a r.world.actors)

-- | What an Actor bought (positive) or sold (negative) of a Resource in a round's Exchange fills.
traded :: RoundResult -> ActorId -> ResourceId -> Q
traded r a res = foldl (\s f -> Q.add s f.quantity) Q.zero (filter (\f -> f.actor == a && f.resource == res) (select exchangeFills r.record.events))

-- | What an Actor received in Money for its sales of a Resource in a round.
receipts :: RoundResult -> ActorId -> ResourceId -> Q
receipts r a res = foldl (\s f -> Q.add s (Q.neg (Q.mul f.price f.quantity))) Q.zero (filter (\f -> f.actor == a && f.resource == res && f.quantity < Q.zero) (select exchangeFills r.record.events))

shortOf :: RoundResult -> ActorId -> ResourceId -> Q
shortOf r a res = foldl (\s f -> Q.add s f.quantity) Q.zero (filter (\f -> f.actor == a && f.resource == res) (select shortfalls r.record.events))

blocked :: RoundResult -> Int
blocked r = length (filter isBlocked (select (event @"OrderRemoved") r.record.events) <> filter isBlocked (select (event @"Rejected") r.record.events))
  where
  isBlocked x = case x.reason of
    Blocked _ -> true
    _ -> false

totalMoney :: RoundResult -> Q
totalMoney r = Q.add (holdingAt r p.us p.money) (holdingAt r p.cn p.money)

spec :: Spec Unit
spec = describe "Pacific (docs/05 §11)" do
  it "A Free trade: 4 T and 6 I cross each round; Money 92 and 108 after round 1; no shortfall" do
    r <- play { us: presetId.freeTrade, cn: presetId.freeTrade } 6
    r1 <- roundOf r 1
    traded r1 p.us p.technology `shouldEqual` Q.fromInt (-4)
    traded r1 p.cn p.industry `shouldEqual` Q.fromInt (-6)
    holdingAt r1 p.us p.money `shouldEqual` Q.fromInt 92
    holdingAt r1 p.cn p.money `shouldEqual` Q.fromInt 108
    for_ r.records \x -> do
      traded x p.us p.technology `shouldEqual` Q.fromInt (-4)
      traded x p.us p.industry `shouldEqual` Q.fromInt 6
      shortOf x p.us p.industry `shouldEqual` Q.fromInt 0
      shortOf x p.cn p.technology `shouldEqual` Q.fromInt 0
      totalMoney x `shouldEqual` Q.fromInt 200
    r6 <- roundOf r 6
    holdingAt r6 p.us p.money `shouldEqual` Q.fromInt 52

  it "B US technology ban: from round 2 no T crosses, China is short 4, I trade continues" do
    r <- play { us: presetId.restrict, cn: presetId.freeTrade } 3
    r2 <- roundOf r 2
    traded r2 p.us p.technology `shouldEqual` Q.fromInt 0
    traded r2 p.cn p.technology `shouldEqual` Q.fromInt 0
    shortOf r2 p.cn p.technology `shouldEqual` Q.fromInt 4
    receipts r2 p.us p.technology `shouldEqual` Q.fromInt 0
    traded r2 p.cn p.industry `shouldEqual` Q.fromInt (-6)
    holdingAt r2 p.us p.technology `shouldEqual` Q.fromInt 4
    (blocked r2 > 0) `shouldEqual` true
    r1 <- roundOf r 1
    Map.size r1.world.rules `shouldEqual` 0
    Map.size r2.world.rules `shouldEqual` 1

  it "C China's industrial ban: from round 2 no I crosses, the US is short 6, T trade continues" do
    r <- play { us: presetId.freeTrade, cn: presetId.restrict } 3
    r2 <- roundOf r 2
    traded r2 p.cn p.industry `shouldEqual` Q.fromInt 0
    shortOf r2 p.us p.industry `shouldEqual` Q.fromInt 6
    receipts r2 p.cn p.industry `shouldEqual` Q.fromInt 0
    traded r2 p.us p.technology `shouldEqual` Q.fromInt (-4)

  it "D US substitution: 2 T invested, round 1 exports 2, first extra I in round 3, imports 6 to 4, China's receipts 12 to 8" do
    r <- playRules { us: { rules: investOnce, sign: [] }, cn: noPolicy } 6
    r1 <- roundOf r 1
    traded r1 p.us p.technology `shouldEqual` Q.fromInt (-2)
    r2 <- roundOf r 2
    holdingAt r2 p.us p.industryCapacity `shouldEqual` Q.fromInt 2
    traded r2 p.us p.industry `shouldEqual` Q.fromInt 6
    r3 <- roundOf r 3
    traded r3 p.us p.industry `shouldEqual` Q.fromInt 4
    receipts r3 p.cn p.industry `shouldEqual` Q.fromInt 8
    shortOf r3 p.us p.industry `shouldEqual` Q.fromInt 0

  it "E China's substitution: 4 I invested, round 1 exports 2, first extra T in round 4, imports 4 to 2, US receipts 4 to 2" do
    r <- playRules { us: noPolicy, cn: { rules: investOnce, sign: [] } } 6
    r1 <- roundOf r 1
    traded r1 p.cn p.industry `shouldEqual` Q.fromInt (-2)
    r3 <- roundOf r 3
    holdingAt r3 p.cn p.technologyCapacity `shouldEqual` Q.fromInt 2
    traded r3 p.cn p.technology `shouldEqual` Q.fromInt 4
    r4 <- roundOf r 4
    traded r4 p.cn p.technology `shouldEqual` Q.fromInt 2
    receipts r4 p.us p.technology `shouldEqual` Q.fromInt 2
    shortOf r4 p.cn p.technology `shouldEqual` Q.fromInt 0

  it "F Ban and adaptation: China's technology shortfall goes 4, 4, 4, 2, 0 under a continuing US ban" do
    r <- play { us: presetId.restrict, cn: presetId.adaptive } 6
    map (\x -> shortOf x p.cn p.technology) r.records `shouldEqual` map Q.fromInt [ 0, 4, 4, 4, 2, 0 ]
    r6 <- roundOf r 6
    holdingAt r6 p.cn p.technologyCapacity `shouldEqual` Q.fromInt 4
    traded r6 p.cn p.technology `shouldEqual` Q.fromInt 0

  it "F reversed: the US shortfall of I goes 6, 6, 4, 2, 0 as it substitutes under China's ban" do
    r <- play { us: presetId.adaptive, cn: presetId.restrict } 6
    map (\x -> shortOf x p.us p.industry) r.records `shouldEqual` map Q.fromInt [ 0, 6, 6, 4, 2, 0 ]

  it "G No phantom trade: with no US demand, China's offered I earns nothing and stays with China" do
    r <- play { us: presetId.substitute, cn: presetId.freeTrade } 6
    r5 <- roundOf r 5
    traded r5 p.us p.industry `shouldEqual` Q.fromInt 0
    receipts r5 p.cn p.industry `shouldEqual` Q.fromInt 0
    holdingAt r5 p.cn p.industry `shouldEqual` Q.fromInt 12
    holdingAt r5 p.cn p.money `shouldEqual` Q.fromInt 122

  it "H Replay and submission order reproduce every scenario" do
    parts <- pacificParts
    for_ [ Tuple presetId.restrict presetId.adaptive, Tuple presetId.restrictAndSubstitute presetId.restrictAndSubstitute ] \(Tuple us cn) -> do
      usRules <- preset parts us
      cnRules <- preset parts cn
      g <- orThrow (assemble (FromComposition parts) (strategyRulesBindings [ Tuple p.us usRules, Tuple p.cn cnRules ]) [])
      a <- run (Seed 1) g
      out <- orThrow (runReplay (seatEnv (Seed 1) g) 6 (map _.record a.records) g)
      map _.world out.records `shouldEqual` map _.world a.records
      let befores = [ a.manifest.initial ] <> map _.world a.records
      for_ (zip befores a.records) \(Tuple before x) -> orderIndependent before x `shouldEqual` []

  it "A human seat plays its routine after its speech: commerce sells only what the spoken investment leaves" do
    parts <- pacificParts
    g <- orThrow (assemble (FromComposition parts) (liveBindings [ p.us ] parts.config) [])
    case Map.lookup p.us g.seats, Map.lookup p.us g.world.actors of
      Just b@(Composed _), Just us -> do
        let
          spoken = [ Transform { by: p.us, transformation: p.usIndustrialInvestment, quantity: Q.fromInt 1 } ]
          sells v = foldl
            ( \s c -> foldl
                ( \t i -> case i of
                    Trade x | x.quantity < Q.zero -> Q.add t x.quantity
                    _ -> t
                )
                s
                c.intents
            )
            Q.zero
            (proposeAnswer (seatEnv (Seed 1) g) p.us v b)
          view = observe g.world us
        sells view `shouldEqual` Q.fromInt (-4)
        sells (view { proposed = spoken }) `shouldEqual` Q.fromInt (-2)
        -- an order above the maximum of 1 holds back only what 1 unit uses, since no more can settle
        sells (view { proposed = [ Transform { by: p.us, transformation: p.usIndustrialInvestment, quantity: Q.fromInt 4 } ] }) `shouldEqual` Q.fromInt (-2)
      _, _ -> throwError (error "the US seat is not composed with its routine")

  it "Sensitivity (docs/05 §12): over prices, investment cost, lead time, requirements and opening Money, a ban bites, substitution ends it, and substitution costs the exporter its sales" do
    let
      variants =
        [ Pacific.defaultDials
        , Pacific.defaultDials { technologyPrice = Q.fromInt 2 }
        , Pacific.defaultDials { industryPrice = Q.fromInt 1 }
        , Pacific.defaultDials { industryPrice = Q.fromInt 3 }
        , Pacific.defaultDials { cnInvestment = { cost: Q.fromInt 2, yield: Q.fromInt 2, delay: 2 } }
        , Pacific.defaultDials { cnInvestment = { cost: Q.fromInt 6, yield: Q.fromInt 2, delay: 2 } }
        , Pacific.defaultDials { cnInvestment = { cost: Q.fromInt 4, yield: Q.fromInt 2, delay: 1 } }
        , Pacific.defaultDials { cnInvestment = { cost: Q.fromInt 4, yield: Q.fromInt 2, delay: 3 } }
        , Pacific.defaultDials { technologyNeed = { us: Q.fromInt 6, cn: Q.fromInt 2 } }
        , Pacific.defaultDials { technologyNeed = { us: Q.fromInt 5, cn: Q.fromInt 5 } }
        , Pacific.defaultDials { money = { us: Q.fromInt 60, cn: Q.fromInt 140 } }
        ]
    for_ variants \d -> do
      source <- orThrow (parts (Pacific.pacific d))
      let
        playOn us cn = do
          usRules <- preset source us
          cnRules <- preset source cn
          g <- orThrow (assemble (FromComposition source) (strategyRulesBindings [ Tuple p.us usRules, Tuple p.cn cnRules ]) [])
          r <- run (Seed 1) (withRounds (Just 10) g)
          checkRun r.manifest.initial r.records `shouldEqual` []
          pure r
      free <- playOn presetId.freeTrade presetId.freeTrade
      banned <- playOn presetId.restrict presetId.freeTrade
      adapted <- playOn presetId.restrict presetId.adaptive
      let
        cnShort r = map (\x -> shortOf x p.cn p.technology) r.records
        usTechReceipts r = foldl Q.add Q.zero (map (\x -> receipts x p.us p.technology) r.records)
      -- free trade meets every requirement while each side's Money lasts (six rounds in every variant; docs/05 §12);
      -- a ban leaves China short from round 2
      foldl Q.add Q.zero (take 6 (cnShort free)) `shouldEqual` Q.fromInt 0
      (fromMaybe Q.zero (index (cnShort banned) 1) > Q.zero) `shouldEqual` true
      -- substitution ends the shortfall before the run does, and the ban then costs China nothing more
      (fromMaybe Q.zero (index (cnShort adapted) 9)) `shouldEqual` Q.fromInt 0
      -- substitution under free trade costs the US its technology sales
      substituted <- playOn presetId.freeTrade presetId.substitute
      (usTechReceipts substituted < usTechReceipts free) `shouldEqual` true

  it "A levy on a matched pair: a US import levy of 1 M per I from China charges China once per shipment, 6 M in round 1" do
    parts <- pacificParts
    freeTrade <- preset parts presetId.freeTrade
    let
      levyKind = Id "us-import-levy"
      levied = parts
        { composition = parts.composition
            { world = parts.composition.world
                { ruleKinds = Map.insert levyKind { form: LevyFlows GovernedByDestination, text: "{governor} levies {counterparty} goods" } parts.composition.world.ruleKinds
                , rules = Map.singleton (Id "levy-1") (FlowRule { kind: levyKind, scope: { governor: p.us, counterparty: p.cn, direction: Inbound, over: [ p.industry ] }, effect: Levy { rate: Q.fromInt 1, basis: PerUnit, in: p.money, to: LevyToGovernor } })
                }
            }
        }
    g <- orThrow (assemble (FromComposition levied) (strategyRulesBindings [ Tuple p.us freeTrade, Tuple p.cn freeTrade ]) [])
    r <- run (Seed 1) (withRounds (Just 1) g)
    checkRun r.manifest.initial r.records `shouldEqual` []
    r1 <- roundOf r 1
    traded r1 p.cn p.industry `shouldEqual` Q.fromInt (-6)
    holdingAt r1 p.cn p.money `shouldEqual` Q.fromInt 102
    holdingAt r1 p.us p.money `shouldEqual` Q.fromInt 98
    length (select levies r1.record.events) `shouldEqual` 1

  it "H A ban enacted twice is in force once" do
    parts <- pacificParts
    restrict <- preset parts presetId.restrict
    let twice = { rules: [ rule [] [ asTemplate (ChangeRule (Enact usBan)) ], rule [] [ asTemplate (ChangeRule (Enact usBan)) ] ] <> restrict.rules, sign: [] }
    r <- playRules { us: twice, cn: noPolicy } 3
    r3 <- roundOf r 3
    Map.size r3.world.rules `shouldEqual` 1

  it "H A ban cannot be bypassed by a gift Transfer, a joint candidate Transaction, or a routed purchase" do
    let
      gift = rule [ eqQ roundQ (lit 2) ] [ asTemplate (Transfer { from: selfActor, to: p.cn, resource: p.technology, quantity: lit 3 }) ]
      jointGift = rule [ eqQ roundQ (lit 2) ] [ joint { signers: [ p.cn ], transfers: [ { from: selfActor, to: p.cn, resource: p.technology, quantity: lit 1 } ], transforms: [] } ]
      route = rule [] [ asTemplate (Route { pair: { base: p.technology, quote: p.money }, quantity: lit 4, limitPrice: Just (lit 1) }) ]
    parts <- pacificParts
    restrict <- preset parts presetId.restrict
    r <- playRules { us: { rules: [ gift, jointGift ] <> restrict.rules, sign: [] }, cn: { rules: [ route ], sign: [ signAll ] } } 2
    r2 <- roundOf r 2
    holdingAt r2 p.cn p.technology `shouldEqual` Q.fromInt 0
    shortOf r2 p.cn p.technology `shouldEqual` Q.fromInt 4
    (blocked r2 >= 3) `shouldEqual` true
  where
  usBan = banRule p.us
