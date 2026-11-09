-- path: packages/laws/src/Laws/Engine/X.purs
f = case _ of
  Settled t -> t.quantity
  _ -> zero
