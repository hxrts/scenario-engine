-- path: packages/x/src/X.purs
-- _ -> Nothing in a comment of a sumOf
aOf = case _ of
  A r -> Just r
  _ -> Nothing
