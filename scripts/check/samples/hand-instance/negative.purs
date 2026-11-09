-- path: packages/x/src/X.purs
derive instance Eq Foo
instance Show Foo where
  show = genericShow
