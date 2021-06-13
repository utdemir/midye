{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Midye.ANSI.Types
  ( TermBytes (..),
    TermSpecial (..),
    TSUnknown (..),
    Style (..),
    styleUnderlined,
    styleBold,
    styleForegroundColor,
    Cell (..),
    cellStyle,
    cellContent,
    cellTouched,
    Row (..),
    rowCells,
    rowEnd,
    RowEnd (..),
    VTY (..),
    vtyScreen,
    vtyState,
    vtyCursor,
    vtySize,
    vtyWidth,
    vtyHeight,
    vtyCursorRow,
    vtyCursorCol,
  )
where

import "this" Data.Color
import "optics-th" Optics.TH qualified

-- * Parser

data TermBytes
  = TBPlain Char
  | TBSpecial TermSpecial
  deriving stock (Show, Eq)

data TermSpecial
  = -- control chars
    TS_BEL
  | TS_BS
  | TS_HT
  | TS_LF
  | TS_CR
  | TS_SO
  | TS_SI
  | TS_CAN
  | TS_ESC
  | TS_DEL
  | TS_CSI
  | -- Non CSI sequences
    TS_RIS
  | TS_IND
  | TS_NEL
  | TS_HTS
  | TS_RI
  | TS_DECPNM
  | TS_DECPAM
  | -- CSI sequences
    TS_SGR [Int]
  | TS_DECSTBM [Int]
  | TS_DECCKM Bool
  | TS_DECTCEM Bool
  | TS_CUP [Int]
  | TS_CUU [Int]
  | TS_CUD [Int]
  | TS_CUF [Int]
  | TS_CUB [Int]
  | TS_ED [Int]
  | TS_EL [Int]
  | TS_StartBlinkingCursor Bool
  | TS_X11MouseReporting Bool
  | TS_BracketedPasteMode Bool
  | -- unknown
    TSUnknown TSUnknown
  deriving stock (Show, Eq)

data TSUnknown
  = TSUnknownCSISequence Bool Char [Int]
  | TSUnknownDECSequence Integer Bool
  deriving stock (Show, Eq)

-- * Printer

data Style = Style
  { _styleUnderlined :: Bool,
    _styleBold :: Bool,
    _styleForegroundColor :: Color
  }

Optics.TH.makeLenses ''Style

data Cell = Cell
  { _cellStyle :: Style,
    _cellContent :: Char,
    _cellTouched :: Bool
  }

Optics.TH.makeLenses ''Cell

data RowEnd
  = RowEndWrapped
  | RowEndNewline Int
  | RowEndNo
  deriving stock (Eq, Show)

data Row = Row
  { _rowCells :: Seq Cell,
    _rowEnd :: RowEnd
  }

Optics.TH.makeLenses ''Row

-- TODO:
-- We probably will need to store whether a line ends with newline or not to
-- handle resizes.
data VTY = VTY
  { _vtyScreen :: Seq Row,
    _vtyCursor :: (Int, Int),
    _vtyState :: Style,
    _vtySize :: (Int, Int)
  }

{-
* 'vtySize' is of shape (height, width).
* 'vtyScreen' is a dense matrix with size 'vtySize', row-major order.
* 'vtyCursor' is of shape (row, col). The row is always smaller than the height, however
  col can be smaller or equal to the width (when col == width, the cursor is not visible
  and there is no corresponding cell on 'vtyScreen').
-}

Optics.TH.makeLenses ''VTY

vtyHeight, vtyWidth :: Lens' VTY Int
vtyHeight = vtySize % _1
vtyWidth = vtySize % _2

vtyCursorRow, vtyCursorCol :: Lens' VTY Int
vtyCursorRow = vtyCursor % _1
vtyCursorCol = vtyCursor % _2
