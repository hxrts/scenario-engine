-- path: packages/x/src/X.purs
import Data.Argonaut.Encode (encodeJson)
-- encodeJson in a comment
f = "encodeJson in a string"
g x = stringify (encodeWith Snapshot fooShape x)
