{-# LANGUAGE QuasiQuotes #-}

module Tests.Midye.ANSI.Widget where

import "string-interpolate" Data.String.Interpolate (__i)
import "hedgehog" Hedgehog
import "this" Midye.ANSI.Types
import "this" Midye.ANSI.Widget
import "tasty" Test.Tasty
import "tasty-hedgehog" Test.Tasty.Hedgehog (testProperty)
import "this" Tests.Util

exampleWidget :: (Int, Int) -> Char -> Widget
exampleWidget size c =
  mkWidget size (\_ -> Just $ touchedEmptyCell & cellContent .~ c)

cases :: [(String, Widget, String)]
cases =
  [ ( "simple",
      exampleWidget (2, 3) 'a',
      [__i|
       aaa
       aaa
      |]
    ),
    ( "right align",
      exampleWidget (2, 3) 'a'
        & widgetHorizontalAlign .~ HAlignRight
        & widgetSetWidth 6,
      "   aaa\n   aaa"
    ),
    ( "horizontal",
      exampleWidget (3, 2) 'a'
        `widgetStackHorizontally` (exampleWidget (2, 3) 'b' & widgetVerticalAlign .~ VAlignTop),
      [__i|
       aabbb
       aabbb
       aa
      |]
    ),
    ( "vertical",
      exampleWidget (2, 4) 'a'
        `widgetStackVertically` (exampleWidget (2, 3) 'b' & widgetHorizontalAlign .~ HAlignRight),
      [__i|
       aaaa
       aaaa
        bbb
        bbb
      |]
    )
  ]

test_widgets :: [TestTree]
test_widgets =
  [ testProperty
      name
      ( withTests 1 . property $ do
          let rendered = renderWidget Nothing widget
          actual <- evalIO $ tmuxCapture (80, 80) rendered
          actual === expected
      )
    | (name, widget, expected) <- cases
  ]
