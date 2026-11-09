-- path: packages/views/src/Views/X.purs
f = case _ of
  Settled t -> t.quantity
  _ -> zero
