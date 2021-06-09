module Midye.Render where

import Midye.ANSI.Printer

render :: VTY -> IO ()
render scr = do
  putStrLn $ '╔' : replicate (scr ^. vtyWidth) '═' ++ "╗"

  forM_ (scr ^. vtyScreen) $ \row -> do
    putStr "║"
    forM_ row $ \cell -> putStr [cell ^. cellContent]
    putStrLn "║"

  putStrLn $ '╚' : replicate (scr ^. vtyWidth) '═' ++ "╝"
