module Midye.Render where

import Midye.ANSI.Printer

render :: VTY -> IO ()
render scr = do
  putStrLn $ '╔' : replicate (scr ^. vtyWidth) '═' ++ "╗"

  forM_ (reverse $ scr ^. vtyRowsAbove) (renderRow . toList)

  renderRow . toList $
    scr ^. vtyCellsBefore <> scr ^. vtyCellsAfter

  forM_ (scr ^. vtyRowsBelow) (renderRow . toList)
  putStrLn $ '╚' : replicate (scr ^. vtyWidth) '═' ++ "╝"
  where
    renderRow :: [Cell] -> IO ()
    renderRow cells = do
      putStr "║"
      forM_ cells $ \cell -> putStr [cell ^. cellContent]
      putStr $ replicate (scr ^. vtyWidth - length cells) ' '
      putStrLn "║"
