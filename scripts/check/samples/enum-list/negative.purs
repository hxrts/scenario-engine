-- path: packages/x/src/X.purs
module X where

data Colour = Red | Green | Blue

colourShape = genericNameShape "Colour" orList camelName

-- a subset is a choice, not a restatement of the type
warm = [ Red, Green ]
