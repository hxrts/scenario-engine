-- path: packages/rules/src/Rules/Engine/X.purs
f = case _ of
  Coupled t -> t.quantity
  _ -> zero
