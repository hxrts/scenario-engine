-- path: packages/x/src/X.purs
instance EncodeJson Foo where
  encodeJson x = fromString (show x)
