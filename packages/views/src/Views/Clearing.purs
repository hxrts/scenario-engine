-- | The Clearing view (docs/07 §9.2; docs/05 §4): what crossed between countries, round by round, in the clearing
-- | system's three categories, and the Rules that stood at the borders. Generic: a country is an Actor group;
-- | everything outside it, the Exchanges' reserves included, is abroad. The exchange rate is the currency Exchanges'
-- | price, in the Exchanges view, and the current account is in the Finance view.
-- |
-- | - **Trade**: goods that moved from one country to another on the Exchanges.
-- | - **Capital**: currency lent across a border and repaid: principal Transferred, and agreements repaid.
-- | - **Rules**: the flows Rules in force after each round, with the country that governs each and the one it binds.
module Views.Clearing
  ( clearing
  , goodsColumns
  , capitalColumns
  , rulesColumns
  , columns
  ) where

import Prelude

import Core.Kernel.Types (Endpoint(..))
import Core.Map (values)
import Core.Num as Q
import Data.Array (any, concatMap, filter, fromFoldable, nub, sort, zip)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Laws.External (groupOf)
import Laws.Slice (legs, select)
import Laws.Types (ResourceId, RuleF(..), idText)
import Laws.World (Cause(..), Mechanism(..), WorldState)
import Views.Common (Column, StoredRun, Table, col, column, names, quantity, worlds)
import Views.Standings (valueIn)

clearing :: StoredRun -> { goods :: Table, capital :: Table, rules :: Table }
clearing r =
  { goods: { title: "Trade between countries", columns: names ([ col.round ] <> goodsColumns), rows: concatMap goodsRows steps }
  , capital: { title: "Capital between countries", columns: names ([ col.round ] <> capitalColumns), rows: concatMap capitalRows steps }
  , rules: { title: "Rules at the borders", columns: names ([ col.round ] <> rulesColumns), rows: concatMap rulesRows r.rounds }
  }
  where
  steps = zip (worlds r) r.rounds
  countryAt w = case _ of
    Held a -> Just (show (groupOf w a))
    InExchange x -> Just ("the " <> idText x <> " reserve")
    _ -> Nothing
  crossing w l = case countryAt w l.from, countryAt w l.to of
    Just a, Just b | a /= b -> Just (Tuple a b)
    _, _ -> Nothing
  -- every good that moved on an Exchange between two countries, by good and direction
  goodsRows (Tuple before x) =
    let
      w = x.world
      moves = concatMap
        ( \l -> case l.cause, crossing w l of
            OfMatch { exchange }, Just (Tuple a b) | isGood before l.resource -> [ { good: l.resource, from: a, to: b, quantity: l.moved, worth: Q.mul l.moved (priceOf before exchange) } ]
            _, _ -> []
        )
        (select legs x.record.events)
      keys = sort (nub (map (\m -> Tuple (show m.good) (Tuple m.from m.to)) moves))
    in
      map
        ( \(Tuple g (Tuple a b)) ->
            let
              these = filter (\m -> show m.good == g && m.from == a && m.to == b) moves
            in
              [ show x.record.round, g, a, b, quantity (Q.sumQ (map _.quantity these)), quantity (Q.sumQ (map _.worth these)) ]
        )
        keys
  -- what a unit of an Exchange's base was worth in the numeraire at its opening price
  priceOf w x = maybe Q.zero (\m -> Q.mul (Q.add m.cost.intercept (Q.mul m.cost.slope m.position)) (valueIn w m.quote)) (Map.lookup x w.exchanges)
  capitalRows (Tuple before x) =
    let
      w = x.world
      moves = concatMap
        ( \l -> case lendingCause l.cause, crossing w l of
            Just kind, Just (Tuple a b) -> [ { kind, currency: show l.resource, from: a, to: b, worth: Q.mul l.moved (valueIn before l.resource), amount: l.moved } ]
            _, _ -> []
        )
        (select legs x.record.events)
      keys = sort (nub (map (\m -> [ m.kind, m.currency, m.from, m.to ]) moves))
    in
      map
        ( \k ->
            let
              these = filter (\m -> [ m.kind, m.currency, m.from, m.to ] == k) moves
            in
              [ show x.record.round ] <> k <> [ quantity (Q.sumQ (map _.amount these)), quantity (Q.sumQ (map _.worth these)) ]
        )
        keys
  lendingCause = case _ of
    OfIntent { mechanism: Direct } -> Just "lent"
    OfAgreement _ -> Just "repaid"
    _ -> Nothing
  rulesRows x =
    map
      ( \rl -> case rl of
          FlowRule f -> [ show x.record.round, show f.kind, show (groupOf x.world f.scope.governor), show (groupOf x.world f.scope.counterparty) ]
          HoldingRule h -> [ show x.record.round, show h.kind, "", "" ]
      )
      (fromFoldable (values x.world.rules))

-- | A Resource an Exchange trades that no Exchange quotes, and that is not the numeraire: a good.
isGood :: WorldState -> ResourceId -> Boolean
isGood w res = res /= w.numeraire && not (any (\m -> m.quote == res) (values w.exchanges))

goodsColumns :: Array Column
goodsColumns =
  [ column "Good" "A good: a Resource an Exchange trades that is not a currency."
  , column "From" "The country it left: an Actor group, or an Exchange's reserve."
  , column "To" "The country it reached."
  , column "Units" "How much of the good moved that way in the round."
  , column "Worth" "Its worth in the numeraire at the round's opening prices."
  ]

capitalColumns :: Array Column
capitalColumns =
  [ column "Flow" "Lent: a principal Transferred across the border. Repaid: what an agreement moved back."
  , column "Currency" "The currency a bond or an account is in: the numeraire, or a Resource an Exchange quotes."
  , column "From" "The country it left: an Actor group, or an Exchange's reserve."
  , column "To" "The country it reached."
  , column "Amount" "How much of the currency moved that way in the round."
  , column "Worth" "Its worth in the numeraire at the round's opening prices."
  ]

rulesColumns :: Array Column
rulesColumns =
  [ column "Rule kind" "The kind of a Rule in force after the round."
  , column "Governor" "The country that governs the Rule."
  , column "Counterparty" "The country the Rule binds."
  ]

columns :: Array Column
columns = goodsColumns <> capitalColumns <> rulesColumns
