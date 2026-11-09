-- path: packages/x/src/X.purs
f = case _ of
  Trade x -> 1
  Route x -> 2
  Transfer x -> 3
  Transform x -> 4
  ChangeRule _ -> 5
  Enter _ -> 6
