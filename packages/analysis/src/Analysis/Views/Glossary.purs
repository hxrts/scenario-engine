-- | The glossary the dashboard shows: a plain-language definition for every table column, overview
-- | field and term, and what an empty value means (docs/02_type_system.md §2.5, docs/06_simulator.md
-- | §9). Text only, served whole by `GET /api/glossary`; the dashboard looks names up and fills the
-- | `{name}` and `{market}` placeholders of the templates. Pure.
module Analysis.Views.Glossary (Glossary, Reason, glossary) where

import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object

-- | What an empty value shows: a short label, and the reason on hover.
type Reason = { label :: String, reason :: String }

-- | The glossary. `definitions` and `reasons` are keyed by column or field name. `unnamed` is the
-- | entry for a name with none (`{name}` is the name); `price` defines the overview's "Final price"
-- | fields by prefix (`{market}` is the Market).
type Glossary =
  { definitions :: Object String
  , reasons :: Object Reason
  , unnamed :: { definition :: String, reason :: Reason }
  , price :: { prefix :: String, definition :: String }
  }

glossary :: Glossary
glossary =
  { definitions: Object.fromFoldable definitions
  , reasons: Object.fromFoldable reasons
  , unnamed:
      { definition: "The value of {name} for this row. This column is a compared run's id."
      , reason: { label: "not recorded", reason: "The server gave no value for {name} in this run." }
      }
  , price: { prefix: "Final price ", definition: "The market {market} marginal price after the last round, in quote units per base unit." }
  }
  where
  -- "Source" and "Derived from" name one fact: the run this one was derived from
  source = "The run this one was derived from (duplicate, rerun, variant or counterfactual). Empty for an original run."
  noSource = { label: "none", reason: "Original run: launched directly, not derived from another run." }

  definitions =
    [ Tuple "Run" "The run's id in the Run Store. Selecting a row shows that run's results."
    , Tuple "Name" "The name the run was given at launch, or by the derivation that created it."
    , Tuple "Gameboard" "The Gameboard version (scenario) the run was played on."
    , Tuple "Game seed" "The seed fixed by the scenario. It drives the in-world draws, so the same game seed gives the same draws."
    , Tuple "Player seed" "The seed for seat-policy choices. It is separate from the game seed, so the two streams never share draws."
    , Tuple "Rounds" "Rounds completed so far. A run stopped short of its target can be resumed."
    , Tuple "Status" "running, completed or failed. A failed run gives its reason in the Reason column."
    , Tuple "Assertions failing" "Assertion failures across all rounds. An assertion checks an economic invariant (INV-xx), so 0 means none failed."
    , Tuple "Reason" "Why the run failed or cannot be read. Empty for a run that did not fail."
    , Tuple "Archived" "yes if the run is archived, which hides it from the default list. It stays in the store."
    , Tuple "Source" source
    , Tuple "Derived from" source
    , Tuple "Field" "The name of the overview field."
    , Tuple "Value" "The value of the overview field."
    , Tuple "Game ID" "The scenario's game identifier, recorded at launch."
    , Tuple "Engine" "The version of the round engine that resolved each round."
    , Tuple "Draws" "The randomness scheme every draw uses."
    , Tuple "Rounds target" "The number of rounds the run was launched for. A run shorter than this can be resumed to it."
    , Tuple "Rejected Proposals" "Proposals the engine rejected across all rounds. A rejected Proposal has no effect on the economy."
    , Tuple "Military procured" "Military bought through the defense procurement facilities over the run."
    , Tuple "Actor" "The Actor (or Aggregate) this row describes."
    , Tuple "Objective" "A goal from the Gameboard for one Actor. It is measured from the run and never changes the economy."
    , Tuple "Criterion" "The number of the criterion within its objective."
    , Tuple "Outcome" "For objectives: met, not met or not evaluated. A criterion is not evaluated when the run has no rounds or no data, and is never reported as met. For procedures: executed if the support reached the threshold, lapsed otherwise."
    , Tuple "Values" "The measured values for the criterion, in their own units."
    , Tuple "Bound" "The threshold the measured values are compared with."
    , Tuple "Round" "The round this row belongs to. Round 1 is the first round after the initial state."
    , Tuple "Facility" "The facility that filled the order: defense procurement, public attention or energy shipping."
    , Tuple "Ordered by" "The Entity that placed the order."
    , Tuple "Quantity" "The quantity of the facility's output that the order bought."
    , Tuple "Paid" "The Money paid for the order."
    , Tuple "From" "The category of the paying Entity: Actor, or Aggregate (residual)."
    , Tuple "To" "The category of the receiving Entity: Actor, or Aggregate (residual)."
    , Tuple "Money" "Total Money moved from one category to the other over the run (Circulation between Entity categories)."
    , Tuple "Entity" "The Entity whose balance sheet this is: an Actor or an Aggregate."
    , Tuple "Kind" "Actor, or Aggregate (residual): a residual representation of many holders, not one Actor."
    , Tuple "Resource" "The Resource held, for example Money, Energy, Military or Soft Power."
    , Tuple "Initial" "The holding before round 1."
    , Tuple "Current" "The holding after the selected round. It includes any reserved quantity."
    , Tuple "Change" "Current holding minus initial holding."
    , Tuple "Event" "One engine event, listed in the canonical phase order."
    , Tuple "Shortfalls (E)" "Energy wanted but not supplied in the round (a Shortfall). 0 means every request was filled."
    , Tuple "Rejected" "Proposals the engine rejected in the round (rejected Proposals)."
    , Tuple "Lapsed" "Proposals whose signature rule was not met in the round, so they lapsed without executing."
    , Tuple "Consumed (E)" "Energy consumed in the round."
    , Tuple "Decayed" "Quantity that decayed in the round."
    , Tuple "Representative" "The Actor whose purchases the Aggregate represents."
    , Tuple "Aggregate" "The residual Aggregate whose Policy acts on the stored factor."
    , Tuple "Domain" "The market the representation applies to."
    , Tuple "Observation (proposed)" "Stage 1: the representative's proposed purchases in round r, from its validated and escrowed Proposal."
    , Tuple "Executed (own)" "What the representative actually bought in round r."
    , Tuple "Weight" "Estimator weight: how strongly the observation moves the factor."
    , Tuple "Reference" "Estimator reference: the level the observation is compared against."
    , Tuple "Shrinkage" "Estimator shrinkage toward the reference. A larger value means a smaller adjustment."
    , Tuple "Noise spread" "Spread of the noise draw added to the estimate."
    , Tuple "Stored factor" "Stage 2: the adjustment factor stored in round r. The Aggregate's Policy applies it in round r+1."
    , Tuple "Aggregate purchase (r+1)" "Stage 3: the Aggregate's proposed purchase in round r+1 after applying the stored factor. n/a in the last round, which has no next round."
    , Tuple "Influence ratio" "The induced change in Aggregate activity divided by what the representative directly controls. n/a in the last round, or when the representative bought nothing. A balancing parameter, not a claim that the representative is an unbiased sample."
    , Tuple "Proposal" "The procedure's Proposal id."
    , Tuple "Members who signed" "The members whose signatures counted toward the procedure's Proposal."
    , Tuple "Support" "The signed weight behind the procedure's Proposal."
    , Tuple "Threshold" "The weight the Proposal needs to execute."
    , Tuple "Units" "Whether every Resource has the same unit in each run."
    , Tuple "Market definitions" "Whether the Markets are defined the same way in each run."
    , Tuple "Actor selection" "The preset every seat plays. differs means the runs used different presets."
    , Tuple "Comparable with the first" "yes, or no with the reasons. A run flagged no is shown but not combined with the first run."
    , Tuple "Parameter" "A specification setting, compared across the runs."
    , Tuple "Δ" "Difference in holding from the baseline run."
    , Tuple "Alternative" "A counterfactual choice the Actor could have made instead."
    , Tuple "Price change" "Change in the market price the alternative would cause."
    , Tuple "Others' holdings" "Change in the other Actors' holdings under the alternative."
    , Tuple "Aggregate holdings" "Change in the Aggregate's holdings under the alternative."
    , Tuple "Institutional decisions" "Decisions made through the Aggregates' collective procedures under the alternative."
    , Tuple "Own criteria changed" "Whether the Actor's own objective criteria change under the alternative."
    , Tuple "Measurable" "Whether the effect can be measured from the run's records."
    ]

  reasons =
    [ Tuple "Source" noSource
    , Tuple "Derived from" noSource
    , Tuple "Reason" { label: "none", reason: "The run did not fail, so it has no failure reason." }
    , Tuple "Kind" { label: "not recorded", reason: "Runs made before the run kind was recorded have no kind." }
    , Tuple "Game ID" { label: "not recorded", reason: "Runs made before the game ID was recorded have none." }
    , Tuple "Player seed" { label: "not recorded", reason: "The run's stored specification has no player seed (runs made before seeds were recorded)." }
    , Tuple "Game seed" { label: "not recorded", reason: "The run's stored manifest has no game seed." }
    , Tuple "Name" { label: "not recorded", reason: "The run was stored without a name." }
    , Tuple "Source status" { label: "none", reason: "Original run: there is no source run to check." }
    ]
