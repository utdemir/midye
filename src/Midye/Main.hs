module Midye.Main
  ( main,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar (modifyMVar_)
import Data.Duration qualified
import Midye.ANSI.Parser qualified as ANSI.Parser
import Midye.ANSI.Printer qualified as ANSI.Printer
import Midye.Process
import Midye.Render qualified
import Streaming.ByteString qualified as StreamingBS
import Streaming.Prelude qualified as Streaming
import System.Clock qualified as Clock
import System.IO qualified as IO

main :: IO ()
main = do
  putStrLn "Midye!"

  IO.hSetBuffering IO.stdin IO.NoBuffering
  repl

{-@ lazy repl @-}
repl :: IO ()
repl = do
  putText "> "

  IO.hFlush IO.stdout

  IO.hSetEcho IO.stdin True
  cmd <- getLine
  IO.hSetEcho IO.stdin False

  runCommand cmd
  repl

height, width :: Int
height = 20
width = 90

runCommand :: Text -> IO ()
runCommand cmd = do
  startTime <- Clock.getTime Clock.MonotonicRaw

  (stdoutS, stderrS, stdinS, closeHandles, process) <- execWithPty (height, width) "bash" ["-c", toString cmd]

  stdoutVar <- newMVar $ ANSI.Printer.initVTY (height, width)
  stderrVar <- newMVar $ ANSI.Printer.initVTY (height, width)

  t1 <-
    async $
      stdoutS
        & ANSI.Parser.run
        & Streaming.mapM_ (\tb -> modifyMVar_ stdoutVar (return . ANSI.Printer.run tb))
  t2 <-
    async $
      stderrS
        & ANSI.Parser.run
        & Streaming.mapM_ (\tb -> modifyMVar_ stderrVar (return . ANSI.Printer.run tb))

  inVar <- newEmptyMVar @_ @Char
  t4 <-
    async . forever $ do
      c <- IO.getChar
      putMVar inVar c
  t5 <-
    async $
      Streaming.repeatM @IO (takeMVar inVar)
        & Streaming.map (encodeUtf8 . pure @[])
        & StreamingBS.fromChunks
        & stdinS

  exitCode <- waitForProcess process
  endTime <- Clock.getTime Clock.MonotonicRaw

  -- FIXME: Something is not flushing correctly, and we sometimes get missing output.
  -- This makes it better, but we need to figure out the root cause.
  threadDelay 10000

  closeHandles

  mapM_ wait [t1, t2]
  mapM_ cancel [t4, t5]

  putStrLn "stdout:"
  Midye.Render.render =<< readMVar stdoutVar

  putStrLn "stderr:"
  Midye.Render.render =<< readMVar stderrVar

  let took = Clock.toNanoSecs $ Clock.diffTimeSpec endTime startTime
  let tookHuman = Data.Duration.approximativeDuration (fromIntegral @Integer @Data.Duration.Seconds took / 1_000_000_000)
  putStrLn $ mconcat ["Took: ", tookHuman, "."]

  print exitCode
