module Midye.Render where

import Midye.ANSI.Printer

render :: VTY -> IO ()
render scr = do
  putStrLn $ '╔' : replicate (scr ^. vtyWidth) '═' ++ "╗"

  forM_ (scr ^. vtyScreen) $ \row -> do
    putStr "║"
    forM_ (row ^. rowCells) $ \cell -> putStr [cell ^. cellContent]
    putStrLn "║"

  putStrLn $ '╚' : replicate (scr ^. vtyWidth) '═' ++ "╝"

renderDiff :: VTY -> VTY -> IO ()
renderDiff = undefined
