-- path: packages/x/src/X.purs
instance EncodeJson Foo where
  encodeJson = encodeJsonWith Snapshot fooShape
