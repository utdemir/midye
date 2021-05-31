{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Midye.ANSI.Printer
  ( Style (..),
    styleUnderlined,
    styleBold,
    styleForegroundColor,
    Cell (..),
    cellStyle,
    cellContent,
    VTY (..),
    vtyRowsAbove,
    vtyRowsBelow,
    vtyCellsBefore,
    vtyCellsAfter,
    vtySize,
    vtyWidth,
    vtyHeight,
    initVTY,
    run,
  )
where

import "this" Data.Color
import Data.Sequence qualified as Seq
import "this" Midye.ANSI.Parser (ControlCharacter (..), TermBytes (..), TermSpecial (..))
import "optics-th" Optics.TH qualified

data Style = Style
  { _styleUnderlined :: Bool,
    _styleBold :: Bool,
    _styleForegroundColor :: Color
  }

Optics.TH.makeLenses ''Style

initStyle :: Style
initStyle = Style False False white

data Cell = Cell
  { _cellStyle :: Style,
    _cellContent :: Char
  }

Optics.TH.makeLenses ''Cell

-- TODO:
-- We probably will need to store whether a line ends with newline or not to
-- handle resizes.
data VTY = VTY
  { _vtyRowsAbove :: [Seq Cell],
    _vtyCellsBefore :: Seq Cell,
    _vtyCellsAfter :: Seq Cell,
    _vtyRowsBelow :: [Seq Cell],
    _vtyState :: Style,
    _vtySize :: (Int, Int)
  }

Optics.TH.makeLenses ''VTY

initVTY :: (Int, Int) -> VTY
initVTY size =
  VTY
    { _vtySize = size,
      _vtyState = initStyle,
      _vtyRowsAbove = [],
      _vtyCellsBefore = Seq.empty,
      _vtyCellsAfter = Seq.empty,
      _vtyRowsBelow = []
    }

vtyWidth, vtyHeight :: Lens' VTY Int
vtyWidth = vtySize % _1
vtyHeight = vtySize % _2

wrapCurrentRow :: VTY -> VTY
wrapCurrentRow scr =
  if
      | length (scr ^. vtyCellsBefore) >= scr ^. vtyWidth ->
        let (newAbove, newBefore) = Seq.splitAt (scr ^. vtyWidth) (scr ^. vtyCellsBefore)
         in scr
              & vtyRowsAbove %~ cons newAbove
              & vtyCellsBefore .~ newBefore
      | length (scr ^. vtyCellsBefore) + length (scr ^. vtyCellsAfter) >= scr ^. vtyWidth ->
        let loc = scr ^. vtyWidth - length (scr ^. vtyCellsBefore)
            (newAfter, newBelow) = Seq.splitAt loc (scr ^. vtyCellsAfter)
         in scr
              & vtyCellsAfter .~ newAfter
              & vtyRowsBelow %~ cons newBelow
      | otherwise -> scr

run :: TermBytes -> VTY -> VTY
run (TBPlain c) scr =
  let cell = Cell (scr ^. vtyState) c
   in scr
        & vtyCellsBefore %~ (|> cell)
        & wrapCurrentRow
run (TBSpecial (TSControlCharacter CC_CR)) scr =
  scr
    & vtyCellsBefore .~ Seq.empty
    & vtyCellsAfter %~ (scr ^. vtyCellsBefore Seq.><)
run (TBSpecial (TSControlCharacter CC_LF)) scr =
  scr
    & vtyRowsAbove %~ cons (scr ^. vtyCellsBefore <> scr ^. vtyCellsAfter)
    & vtyCellsBefore .~ Seq.empty
    & vtyCellsAfter .~ Seq.empty
run (TBSpecial (TSControlCharacter CC_HT)) scr =
  let currCol = Seq.length (scr ^. vtyCellsBefore)
      requiredSpaces = 8 - currCol `mod` 8
   in scr
        & vtyCellsBefore %~ (<> Seq.replicate requiredSpaces (Cell (scr ^. vtyState) ' '))
        & wrapCurrentRow
run (TBSpecial _) scr =
  scr
