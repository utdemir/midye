module Data.Color where

data Color = Color Int Int Int
  deriving stock (Eq, Show)

black, white :: Color
black = Color 0 0 0
white = Color 255 255 255
