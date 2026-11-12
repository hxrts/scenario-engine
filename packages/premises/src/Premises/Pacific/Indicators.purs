-- | Pacific's own indicator (docs/05 §4.4), which reads its wages and so cannot be a generic view: each country's
-- | substitute gap, what a unit of its substitute costs it in wages less what importing that unit costs it at the
-- | round's prices. Positive, its substitute industry stands idle (a strong-currency trap); negative, it runs.
module Premises.Pacific.Indicators
  ( substituteGap
  ) where

import Prelude

import Core.Exchange (exchangePrice)
import Core.Num (Q)
import Core.Num as Q
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Laws.World (WorldState)
import Premises.Pacific.Country (Country)
import Premises.Pacific.Dials (Dials)
import Premises.Pacific.Names (p)

-- | A country's substitute gap in a World: its substitute's wage less the import cost, both in its own currency.
substituteGap :: Dials -> WorldState -> Country -> Q
substituteGap d w c = Q.sub wage importCost
  where
  wage = if c.state == p.us then d.wage.usIndustry else d.wage.cnTechnology
  price x = fromMaybe Q.zero (exchangePrice <$> Map.lookup x w.exchanges)
  rate = price p.currency
  importCost = if c.money == p.usd then fromMaybe Q.zero (Q.divMaybe (price c.world) rate) else Q.mul (price c.world) rate
