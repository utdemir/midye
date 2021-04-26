{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Midye.Process
  ( execWithPty,

    -- * Re-exports
    Process.waitForProcess,
    ExitCode (..),
  )
where

import "inline-c" Language.C.Inline qualified as C
import "streaming-bytestring" Streaming.ByteString (ByteStream)
import "streaming-bytestring" Streaming.ByteString qualified as StreamingBS
import System.Exit (ExitCode (..))
import System.IO (BufferMode (NoBuffering), hSetBuffering)
import "unix" System.Posix.IO (fdToHandle)
import System.Posix.Types (Fd (..))
import "process" System.Process qualified as Process

C.context (C.baseCtx <> C.bsCtx)

C.include "<pty.h>"
C.include "<stdio.h>"

execWithPty ::
  FilePath ->
  [String] ->
  IO
    ( ByteStream IO (),
      ByteStream IO (),
      ByteStream IO () -> IO (),
      Process.ProcessHandle
    )
execWithPty fp args = do
  (stdoutMaster, stdoutSlave) <- openPty
  (stderrMaster, stderrSlave) <- openPty
  (stdinMaster, stdinSlave) <- openPty
  mapM_ (flip hSetBuffering NoBuffering) [stdoutMaster, stderrMaster, stdinSlave]
  let p =
        (Process.proc fp args)
          { Process.std_out = Process.UseHandle stdoutMaster,
            Process.std_err = Process.UseHandle stderrMaster,
            Process.std_in = Process.UseHandle stdinMaster
          }
  (_, _, _, processHandle) <- Process.createProcess p
  return
    ( StreamingBS.hGetContents stdoutSlave,
      StreamingBS.hGetContents stderrSlave,
      StreamingBS.toHandle stdinSlave,
      processHandle
    )

openPty :: IO (Handle, Handle)
openPty = do
  (masterFd, slaveFd) <-
    C.withPtrs_ @(C.CInt, C.CInt) $ \(master_ptr, slave_ptr) -> do
      [C.block|
        void {
          openpty($(int* master_ptr), $(int* slave_ptr), NULL, NULL, NULL);
        }
      |]
  (,)
    <$> fdToHandle (Fd masterFd)
    <*> fdToHandle (Fd slaveFd)
