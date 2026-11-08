-- path: packages/core/src/Core/Engine/X.purs
f = case _ of
  Traded t -> t.quantity
  _ -> zero
