-- path: packages/x/src/X.purs
fooShape = sumOf "Foo"
  [ ctorOf { tag: "A", desc: "a" } s
      ( \x -> case x of
          A r -> Just r
          _ -> Nothing
      )
      A
  ]
