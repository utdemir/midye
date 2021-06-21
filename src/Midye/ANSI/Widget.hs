{-# LANGUAGE TemplateHaskell #-}

module Midye.ANSI.Widget
  ( Widget,
    mkWidget,
    widgetHeight,
    widgetWidth,
    widgetHorizontalAlign,
    widgetVerticalAlign,
    widgetSetWidth,
    widgetSetHeight,
    VAlign (..),
    HAlign (..),
    hLine,
    renderWidget,
    widgetMakeOpaque,
    widgetStackVertically,
    widgetStackHorizontally,
    widgetOverlay,
  )
where

import "bytestring" Data.ByteString.Char8 qualified as ByteString.Char8
import "text" Data.Text qualified as Text
import "this" Midye.ANSI.Types
import "optics-th" Optics.TH qualified

data VAlign = VAlignBottom | VAlignTop

data HAlign = HAlignLeft | HAlignRight

data Widget = Widget
  { _widgetSizeUnsafe :: (Int, Int),
    _widgetCells :: (Int, Int) -> Maybe Cell,
    _widgetAlign :: (VAlign, HAlign)
  }

Optics.TH.makeLenses ''Widget

widgetHeightUnsafe, widgetWidthUnsafe :: Lens' Widget Int
widgetHeightUnsafe = widgetSizeUnsafe % _1
widgetWidthUnsafe = widgetSizeUnsafe % _2

widgetHeight, widgetWidth :: Lens' Widget Int
widgetHeight = widgetSize % _1
widgetWidth = widgetSize % _2

widgetVerticalAlign :: Lens' Widget VAlign
widgetVerticalAlign = widgetAlign % _1

widgetHorizontalAlign :: Lens' Widget HAlign
widgetHorizontalAlign = widgetAlign % _2

mkWidget :: (Int, Int) -> ((Int, Int) -> Maybe Cell) -> Widget
mkWidget s c = Widget s c (VAlignBottom, HAlignLeft)

widgetSize :: Lens' Widget (Int, Int)
widgetSize =
  lens
    (view widgetSizeUnsafe)
    (\w (r, c) -> w & widgetSetHeight r & widgetSetWidth c)

widgetSetWidth :: Int -> Widget -> Widget
widgetSetWidth newWidth w =
  w
    & widgetWidthUnsafe .~ newWidth
    & widgetCells %~ \oldCells ->
      if w ^. widgetWidth >= newWidth
        then oldCells
        else case w ^. widgetHorizontalAlign of
          HAlignLeft -> \(r, c) ->
            if c < w ^. widgetWidth
              then oldCells (r, c)
              else Nothing
          HAlignRight -> \(r, c) ->
            let diff = newWidth - w ^. widgetWidth
             in if c < diff
                  then Nothing
                  else oldCells (r, c - diff)

widgetSetHeight :: Int -> Widget -> Widget
widgetSetHeight newHeight w =
  w
    & widgetHeightUnsafe .~ newHeight
    & widgetCells %~ \oldCells ->
      if w ^. widgetHeight >= newHeight
        then oldCells
        else case w ^. widgetVerticalAlign of
          VAlignTop -> \(r, c) ->
            if r < w ^. widgetHeight
              then oldCells (r, c)
              else Nothing
          VAlignBottom -> \(r, c) ->
            let diff = newHeight - w ^. widgetHeight
             in if r < diff
                  then Nothing
                  else oldCells (r - diff, c)

-- * Composition

widgetOverlay :: Widget -> Widget -> Widget
widgetOverlay w1 w2 =
  let setSize w =
        w
          & widgetHeight .~ max (w1 ^. widgetHeight) (w2 ^. widgetHeight)
          & widgetWidth .~ max (w1 ^. widgetWidth) (w2 ^. widgetWidth)
      w1' = setSize w1
      w2' = setSize w2
   in w2'
        & widgetCells .~ \pos ->
          (w2' ^. widgetCells $ pos) <|> (w1' ^. widgetCells $ pos)

widgetMakeOpaque :: Widget -> Widget
widgetMakeOpaque =
  widgetCells %~ \f p -> f p <|> Just untouchedCell

widgetStackVertically :: Widget -> Widget -> Widget
widgetStackVertically top bot =
  let newSize =
        ( top ^. widgetHeight + bot ^. widgetHeight,
          max (top ^. widgetWidth) (bot ^. widgetWidth)
        )
   in widgetOverlay
        ( top
            & widgetVerticalAlign .~ VAlignTop
            & widgetSize .~ newSize
        )
        ( bot
            & widgetVerticalAlign .~ VAlignBottom
            & widgetSize .~ newSize
        )

widgetStackHorizontally :: Widget -> Widget -> Widget
widgetStackHorizontally left right =
  let newSize =
        ( max (left ^. widgetHeight) (right ^. widgetHeight),
          left ^. widgetWidth + right ^. widgetWidth
        )
   in widgetOverlay
        ( left
            & widgetHorizontalAlign .~ HAlignLeft
            & widgetSize .~ newSize
        )
        ( right
            & widgetHorizontalAlign .~ HAlignRight
            & widgetSize .~ newSize
        )

-- * Utils

hLine :: Int -> Widget
hLine len = mkWidget (1, len) (\_ -> Just $ touchedEmptyCell & cellContent .~ '─')

-- * Rendering

renderWidget :: Maybe Widget -> Widget -> ByteString
renderWidget Nothing new = foldMap renderOutBytes (OutBytesEraseDisplay : OutBytesGoTo 0 0 : wOut new)
renderWidget (Just old) new
  | old ^. widgetSize /= new ^. widgetSize = renderWidget Nothing new
  | otherwise = foldMap renderOutBytes $ wDiff old new

-- * Impl

data OutBytes
  = OutBytesPlain Char
  | OutBytesGoTo Int Int
  | OutBytesEraseDisplay

renderOutBytes :: OutBytes -> ByteString
renderOutBytes (OutBytesPlain c) = encodeUtf8 (Text.singleton c)
renderOutBytes OutBytesEraseDisplay = "\x1b[2J"
renderOutBytes (OutBytesGoTo row col) =
  mconcat
    [ "\x1b[",
      ByteString.Char8.pack (show (row + 1)),
      ";",
      ByteString.Char8.pack (show (col + 1)),
      "H"
    ]

-- Diffing

wDiff :: Widget -> Widget -> [OutBytes]
wDiff old new =
  flip concatMap [0 .. new ^. widgetHeight - 1] $ \row ->
    let os = map (\col -> fromMaybe untouchedCell $ old ^. widgetCells $ (row, col)) [0 .. old ^. widgetWidth - 1]
        ns = map (\col -> fromMaybe untouchedCell $ new ^. widgetCells $ (row, col)) [0 .. new ^. widgetWidth - 1]
     in case rowDiff row os ns of
          [] -> []
          bs -> OutBytesGoTo row 0 : bs

wOut :: Widget -> [OutBytes]
wOut w =
  flip concatMap [0 .. w ^. widgetHeight - 1] $ \row ->
    let xs = map (\col -> fromMaybe untouchedCell $ w ^. widgetCells $ (row, col)) [0 .. w ^. widgetWidth - 1]
     in OutBytesGoTo row 0 : rowOut xs

rowDiff :: Int -> [Cell] -> [Cell] -> [OutBytes]
rowDiff rowNo oldCells newCells = go 0 0 (zip oldCells newCells)
  where
    go _ _ [] = []
    go idx cur ((old, new) : xs)
      | old == new = go (idx + 1) cur xs
      | cur == idx = OutBytesPlain (new ^. cellContent) : go (idx + 1) (cur + 1) xs
      | otherwise =
        OutBytesGoTo rowNo idx :
        OutBytesPlain (new ^. cellContent) :
        go (idx + 1) (idx + 1) xs

rowOut :: [Cell] -> [OutBytes]
rowOut = go
  where
    go [] = []
    go (x : xs) = OutBytesPlain (x ^. cellContent) : go xs
