module Midye.Main
  ( main,
  )
where

import Control.Concurrent.Async
import Midye.ANSI.Parser qualified as ANSI.Parser
import Midye.Process
import Streaming.ByteString qualified as StreamingBS
import Streaming.Prelude qualified as Streaming
import System.IO qualified as IO

main :: IO ()
main = do
  putStrLn "Midye!"

  IO.hSetBuffering IO.stdin IO.NoBuffering
  repl

repl :: IO ()
repl = do
  putText "> "

  IO.hFlush IO.stdout

  IO.hSetEcho IO.stdin True
  cmd <- getLine
  IO.hSetEcho IO.stdin False

  runCommand cmd
  repl

runCommand :: Text -> IO ()
runCommand cmd = do
  (stdoutS, stderrS, stdinS, process) <- execWithPty "zsh" ["-c", toString cmd]

  outVar <- newEmptyMVar @_ @(Text, ANSI.Parser.TermBytes)
  t1 <-
    async $
      stdoutS
        & ANSI.Parser.run
        & Streaming.mapM_ (\v -> putMVar outVar ("out", v))
  t2 <-
    async $
      stderrS
        & ANSI.Parser.run
        & Streaming.mapM_ (\v -> putMVar outVar ("err", v))

  t3 <- async . forever $ do
    v <- takeMVar outVar
    print v

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

  wait t1
  wait t2
  exitCode <- waitForProcess process
  cancel t3
  cancel t4
  cancel t5

  print exitCode
