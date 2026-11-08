-- path: packages/x/src/X.purs
instance EncodeJson Foo where
  encodeJson = encodeWith Snapshot fooShape
