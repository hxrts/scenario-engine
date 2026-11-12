-- path: packages/x/src/X.purs
module X where

data Colour = Red | Green | Blue

colourShape = nameShape "Colour" "red, green or blue" name [ Red, Green, Blue ]
