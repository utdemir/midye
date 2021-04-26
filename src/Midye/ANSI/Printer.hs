module Midye.ANSI.Printer where

import "this" Data.Color

data Style = Style
  { sUnderlined :: Bool,
    sBold :: Bool,
    sForegroundColor :: Color
  }

data Cell = Cell
  { ccStyle :: Style,
    ccContent :: Char
  }

data Screen = Screen
  { ssScreen :: [[Cell]],
    ssScreenSize :: (Int, Int),
    ssCursorPos :: (Int, Int)
  }
