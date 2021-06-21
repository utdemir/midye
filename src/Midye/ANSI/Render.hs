{-# LANGUAGE TemplateHaskell #-}

module Midye.ANSI.Render
  ( Row (..),
    rowCells,
    rowEnd,
    VTY (..),
    vtyScreen,
    vtyCursor,
    vtySize,
    vtyWidth,
    vtyHeight,
    renderVTY,
    initVTY,
    run,
  )
where

import Data.Sequence qualified as Seq
import "this" Midye.ANSI.Types
import "this" Midye.ANSI.Widget
import "optics-th" Optics.TH qualified

data RowEnd
  = RowEndWrapped
  | RowEndNewline Int
  | RowEndNo
  deriving stock (Eq, Show)

data Row = Row
  { _rowCells :: Seq Cell,
    _rowEnd :: RowEnd
  }
  deriving stock (Eq, Show)

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

renderVTY :: VTY -> Widget
renderVTY vty =
  mkWidget
    (vty ^. vtySize)
    (\pos -> Just $ vty & vtyCursor .~ pos & view vtyCurrentCell)

vtyHeight, vtyWidth :: Lens' VTY Int
vtyHeight = vtySize % _1
vtyWidth = vtySize % _2

vtyCursorRow, vtyCursorCol :: Lens' VTY Int
vtyCursorRow = vtyCursor % _1
vtyCursorCol = vtyCursor % _2

vtyRow :: Int -> Lens' VTY Row
vtyRow rowNo =
  lens
    ( \vty ->
        vty ^? vtyScreen % ix rowNo
          & fromMaybe (error "invariant violation: cursor out of bounds.")
    )
    ( \vty row ->
        if rowNo >= vty ^. vtyHeight
          then error "invariant violation: cursor out of bounds."
          else vty & vtyScreen % ix rowNo .~ row
    )

vtyCurrentRow :: Lens' VTY Row
vtyCurrentRow =
  lens
    ( \vty ->
        vty ^? vtyRow (vty ^. vtyCursorRow)
          & fromMaybe (error "invariant violation: cursor out of bounds.")
    )
    ( \vty row ->
        vty & vtyRow (vty ^. vtyCursorRow) .~ row
    )

vtyCurrentCell :: Lens' VTY Cell
vtyCurrentCell =
  lens
    ( \vty ->
        let (row, col) = vty ^. vtyCursor
         in vty ^? vtyScreen % ix row % rowCells % ix col
              & fromMaybe (error "invariant violation: cursor out of bounds.")
    )
    ( \vty cell ->
        let (row, col) = vty ^. vtyCursor
         in vty & vtyScreen % ix row % rowCells % ix col .~ cell
    )

cursorAtTheRightEnd :: VTY -> Bool
cursorAtTheRightEnd vty = vty ^. vtyCursorCol == vty ^. vtyWidth

cursorAtTheBottom :: VTY -> Bool
cursorAtTheBottom vty = vty ^. vtyCursorRow == vty ^. vtyHeight - 1

vtyAddRow :: VTY -> VTY
vtyAddRow vty =
  vty
    -- drop the first row
    & vtyScreen %~ Seq.drop 1
    -- and a new row
    & vtyScreen %~ (Seq.|> Row (Seq.replicate (vty ^. vtyWidth) touchedEmptyCell) RowEndNo)
    -- make sure that cursor does not move
    & vtyCursorRow %~ pred

initVTY :: (Int, Int) -> VTY
initVTY size@(height, width) =
  VTY
    { _vtyState = initStyle,
      _vtyCursor = (0, 0),
      _vtySize = size,
      _vtyScreen = Seq.replicate height (Row (Seq.replicate width untouchedCell) RowEndNo)
    }

run :: TermBytes -> VTY -> VTY
run (TBPlain c) vty =
  vty
    & shiftIfNecessary
    & vtyCurrentCell .~ Cell (vty ^. vtyState) c True
    & vtyCursorCol %~ succ
  where
    shiftIfNecessary v =
      case (cursorAtTheRightEnd v, cursorAtTheBottom v) of
        (False, _) -> v
        (True, False) ->
          v
            & vtyCurrentRow % rowEnd .~ RowEndWrapped
            & vtyCursorCol .~ 0
            & vtyCursorRow %~ succ
        (True, True) ->
          v
            & vtyCurrentRow % rowEnd .~ RowEndWrapped
            & vtyAddRow
            & vtyCursorCol .~ 0
            & vtyCursorRow %~ succ
run (TBSpecial TS_CR) vty =
  vty
    & vtyCursorCol .~ 0
run (TBSpecial TS_LF) vty =
  vty
    & (if cursorAtTheBottom vty then vtyAddRow else id)
    & vtyCurrentRow % rowEnd %~ (\x -> if x == RowEndNo then RowEndNewline (vty ^. vtyCursorCol) else x)
    & vtyCursorRow %~ succ
    & vtyCursorCol .~ 0
run (TBSpecial TS_HT) vty =
  -- horizontal tab behaviour:
  --   * (tmux) it moves the cursor forward to the next tabstop (every 8th column).
  -- however, if the cursor is at the rightmost visible column, or the rightmost
  -- column, it doesn't move.
  --   * on some terminals (kitty), when the cursor is on the rightmost (invisible)
  -- column, a horizontal tab moves the cursor back to the rightmost visible column.
  if vty ^. vtyCursorCol >= vty ^. vtyWidth - 1
    then vty
    else
      let tabstops = [0, 8 .. vty ^. vtyWidth - 1]
       in vty & vtyCursorCol
            %~ ( \curr ->
                   find (> curr) tabstops -- find the next tabstop
                     & fromMaybe (vty ^. vtyWidth - 1) -- or the end column
               )
run (TBSpecial TS_BEL) vty =
  -- bell
  vty
run (TBSpecial TS_BS) vty
  -- backspace
  | vty ^. vtyCursorCol == vty ^. vtyWidth = vty & vtyCursorCol %~ pred
  | vty ^. vtyCursorCol == 0 =
    if vty ^. vtyCursorRow == 0
      then vty
      else
        let previousRowEnd = vty & vtyCursorRow %~ pred & view (vtyCurrentRow % rowEnd)
         in case previousRowEnd of
              RowEndWrapped ->
                vty
                  & vtyCursorRow %~ pred
                  & vtyCursorCol .~ (vty ^. vtyWidth - 1)
              _ ->
                vty
  | otherwise = vty & vtyCursorCol %~ pred
run (TBSpecial TS_LS0) vty =
  -- activates the G1 character set
  vty
run (TBSpecial TS_LS1) vty =
  -- activates the G0 character set
  vty
run (TBSpecial TS_CAN) vty =
  -- abort the escape sequence
  vty
run (TBSpecial TS_ESC) vty =
  -- escape
  vty
run (TBSpecial TS_DEL) vty =
  -- del
  vty
run (TBSpecial TS_CSI) vty =
  -- control sequence indicator
  vty
run (TBSpecial TS_RIS) vty =
  -- reset to initial state
  vty
run (TBSpecial TS_IND) vty =
  -- linefeed. like newline, but keep column position
  vty
run (TBSpecial TS_NEL) vty =
  -- like newline, but doesn't add a line
  vty
run (TBSpecial TS_HTS) vty =
  -- character tabulation set. sets horizontal tab stop at the column
  vty
run (TBSpecial TS_RI) vty =
  -- move up one line keeping column position
  vty
run (TBSpecial TS_DECPNM) vty =
  -- set numeric keypad mode
  vty
run (TBSpecial TS_DECPAM) vty =
  -- set application keypad mode
  vty
run (TBSpecial (TS_SGR _params)) vty =
  -- set graphics rendition
  vty
run (TBSpecial (TS_DECSTBM _set)) vty =
  -- set scrolling region
  vty
run (TBSpecial (TS_DECCKM _set)) vty =
  -- application cursor keys
  vty
run (TBSpecial (TS_DECTCEM _set)) vty =
  -- show cursor
  vty
run (TBSpecial (TS_CUP row col)) vty =
  vty
    & vtyCursorRow .~ clampRow vty (row - 1)
    & vtyCursorCol .~ clampCol vty (col - 1)
run (TBSpecial (TS_CUU count)) vty =
  -- cursor up
  vty
    & vtyCursorRow %~ clampRow vty . subtract count
    -- When the cursor is at the (rightmost) invisible column,
    -- CUU moves it one col back to the visible column. This
    -- applies to CUD, CUF, and CUB too.
    & vtyCursorCol %~ clampCol vty
run (TBSpecial (TS_CUD count)) vty =
  -- cursor down
  vty
    & vtyCursorRow %~ clampRow vty . (+ count)
    & vtyCursorCol %~ clampCol vty
run (TBSpecial (TS_CUF count)) vty =
  -- cursor forward
  vty
    & vtyCursorCol %~ clampCol vty . (+ count)
run (TBSpecial (TS_CUB count)) vty =
  -- cursor backward
  vty
    & vtyCursorCol %~ clampCol vty . subtract count
run (TBSpecial (TS_CHA col)) vty =
  -- cursor character absolute
  vty
    & vtyCursorCol .~ clampCol vty (col - 1)
run (TBSpecial (TS_LPA row)) vty =
  -- line position absolute
  vty
    & vtyCursorRow .~ clampRow vty (row - 1)
run (TBSpecial (TS_ED _params)) vty =
  -- erase in display.
  vty
run (TBSpecial (TS_EL param)) vty =
  let vty' = vty & vtyCursorCol %~ clampCol vty
      (pre', _ Seq.:<| post) = Seq.splitAt (vty' ^. vtyCursorCol) (vty' ^. vtyCurrentRow % rowCells)
   in case param of
        -- delete to end of line
        EraseInLineToEnd ->
          vty'
            & vtyCurrentRow % rowCells
              .~ pre' <> Seq.replicate (Seq.length post + 1) untouchedCell
        -- delete to beginning of line
        EraseInLineToBeginning ->
          vty'
            & vtyCurrentRow % rowCells
              .~ Seq.replicate (Seq.length pre' + 1) untouchedCell <> post
        -- delete line
        EraseInLineAll ->
          vty'
            & vtyCurrentRow % rowCells %~ \cs -> Seq.replicate (Seq.length cs) untouchedCell
run (TBSpecial (TS_StartBlinkingCursor _params)) vty =
  vty
run (TBSpecial (TS_X11MouseReporting _params)) vty =
  vty
run (TBSpecial (TS_BracketedPasteMode _params)) vty =
  vty
run (TBSpecial (TS_SCS0 _param)) vty =
  vty
run (TBSpecial (TS_SCS1 _param)) vty =
  vty
run (TBSpecial (TS_SCS2 _param)) vty =
  vty
run (TBSpecial (TS_SCS3 _param)) vty =
  vty
run (TBSpecial (TSUnknown _)) vty =
  vty

clampRow, clampCol :: VTY -> Int -> Int
clampRow vty i = 0 `max` i `min` (vty ^. vtyHeight - 1)
clampCol vty i = 0 `max` i `min` (vty ^. vtyWidth - 1)
