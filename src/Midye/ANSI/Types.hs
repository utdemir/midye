{-# LANGUAGE TemplateHaskell #-}

module Midye.ANSI.Types
  ( TermBytes (..),
    TermSpecial (..),
    TSUnknown (..),
    EraseInLineParam (..),
    Style (..),
    styleUnderlined,
    styleBold,
    styleForegroundColor,
    initStyle,
    Cell (..),
    cellStyle,
    cellContent,
    cellTouched,
    untouchedCell,
    touchedEmptyCell,
  )
where

import "this" Data.Color
import "optics-th" Optics.TH qualified

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
  | TS_LS0
  | TS_LS1
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
  | TS_SCS0 Char
  | TS_SCS1 Char
  | TS_SCS2 Char
  | TS_SCS3 Char
  | -- CSI sequences
    TS_SGR [Int]
  | TS_DECSTBM [Int]
  | TS_DECCKM Bool
  | TS_DECTCEM Bool
  | TS_CUP Int Int
  | TS_CUU Int
  | TS_CUD Int
  | TS_CUF Int
  | TS_CUB Int
  | TS_CHA Int
  | TS_LPA Int
  | TS_ED [Int]
  | TS_EL EraseInLineParam
  | TS_StartBlinkingCursor Bool
  | TS_X11MouseReporting Bool
  | TS_BracketedPasteMode Bool
  | -- unknown
    TSUnknown TSUnknown
  deriving stock (Show, Eq)

data EraseInLineParam
  = EraseInLineToBeginning
  | EraseInLineToEnd
  | EraseInLineAll
  deriving stock (Show, Eq)

data TSUnknown
  = TSUnknownCSISequence Bool Char [Int]
  | TSUnknownDECSequence Integer Bool
  deriving stock (Show, Eq)

-- * Rendering utilities

data Style = Style
  { _styleUnderlined :: Bool,
    _styleBold :: Bool,
    _styleForegroundColor :: Color
  }
  deriving stock (Eq, Show)

Optics.TH.makeLenses ''Style

data Cell = Cell
  { _cellStyle :: Style,
    _cellContent :: Char,
    _cellTouched :: Bool
  }
  deriving stock (Eq, Show)

Optics.TH.makeLenses ''Cell

initStyle :: Style
initStyle = Style False False white

untouchedCell :: Cell
untouchedCell = Cell initStyle ' ' False

touchedEmptyCell :: Cell
touchedEmptyCell = Cell initStyle ' ' False
