module Midye.Main
  ( main,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar (modifyMVar_)
import Data.ByteString qualified as ByteString
import Data.Duration qualified
import Midye.ANSI.Parser qualified as ANSI.Parser
import Midye.ANSI.Render qualified as ANSI.Render
import Midye.ANSI.Widget qualified as ANSI.Widget
import Midye.Process
import Streaming.ByteString qualified as StreamingBS
import Streaming.Prelude qualified as Streaming
import System.Clock qualified as Clock
import System.Console.Terminal.Size qualified as TerminalSize
import System.Environment (getArgs)
import System.IO qualified as IO

main :: IO ()
main = do
  putStrLn "Midye!"
  putStrLn "==================="

  (cmd, args) <-
    getArgs >>= \case
      [] -> error "no args given"
      (x : xs) -> return (x, xs)

  IO.hSetBuffering IO.stdin IO.NoBuffering
  IO.hSetEcho IO.stdin False
  runCommand cmd args

runCommand :: FilePath -> [String] -> IO ()
runCommand cmd args = do
  startTime <- Clock.getTime Clock.MonotonicRaw

  Just (TerminalSize.Window totalHeight totalWidth) <- TerminalSize.size

  let termHeight = totalHeight - 2
      termWidth = totalWidth
  (stdoutS, stderrS, stdinS, closeHandles, process) <- execWithPty (termHeight - 2, termWidth) cmd args

  stdoutVar <- newMVar $ ANSI.Render.initVTY (termHeight, termWidth)
  stderrVar <- newMVar $ ANSI.Render.initVTY (termHeight, termWidth)

  t1 <-
    async $
      stdoutS
        & ANSI.Parser.run
        & Streaming.mapM_ (\tb -> modifyMVar_ stdoutVar (return . ANSI.Render.run tb))
  t2 <-
    async $
      stderrS
        & ANSI.Parser.run
        & Streaming.mapM_ (\tb -> modifyMVar_ stderrVar (return . ANSI.Render.run tb))

  stdinVar <- newEmptyMVar @_ @Char
  t4 <-
    async . forever $ do
      c <- IO.getChar
      putMVar stdinVar c
  t5 <-
    async $
      Streaming.repeatM @IO (takeMVar stdinVar)
        & Streaming.map (encodeUtf8 . pure @[])
        & StreamingBS.fromChunks
        & stdinS

  t6 <- async $ displayThread stdoutVar

  exitCode <- waitForProcess process
  endTime <- Clock.getTime Clock.MonotonicRaw

  -- FIXME: Something is not flushing correctly, and we sometimes get missing output.
  -- This makes it better, but we need to figure out the root cause.
  threadDelay 10000

  closeHandles

  mapM_ wait [t1, t2]
  mapM_ cancel [t4, t5]
  threadDelay 1_000_000
  cancel t6

  let took = Clock.toNanoSecs $ Clock.diffTimeSpec endTime startTime
  let tookHuman = Data.Duration.approximativeDuration (fromIntegral @Integer @Data.Duration.Seconds took / 1_000_000_000)
  putStrLn $ mconcat ["Took: ", tookHuman, "."]

  print exitCode

displayThread :: MVar ANSI.Render.VTY -> IO ()
displayThread stdoutVar = go Nothing
  where
    go :: Maybe ANSI.Widget.Widget -> IO ()
    go old = do
      curr <- readMVar stdoutVar
      let term = ANSI.Render.renderVTY curr
          widget =
            term
              `ANSI.Widget.widgetStackVertically`
              ANSI.Widget.hLine (term ^. ANSI.Widget.widgetWidth)
          diff = ANSI.Widget.renderWidget old widget
      ByteString.hPut stdout diff
      threadDelay 10000
      go (Just widget)
