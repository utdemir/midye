{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Midye.Process
  ( execWithPty,

    -- * Re-exports
    Process.waitForProcess,
    ExitCode (..),
  )
where

import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (Ptr)
import "inline-c" Language.C.Inline qualified as C
import "streaming-bytestring" Streaming.ByteString (ByteStream)
import "streaming-bytestring" Streaming.ByteString qualified as StreamingBS
import System.Exit (ExitCode (..))
import System.IO (hClose, hSetBuffering, BufferMode (NoBuffering), hFlush)
import "unix" System.Posix.IO (fdToHandle)
import System.Posix.Types (Fd (..))
import "process" System.Process qualified as Process

C.context (C.baseCtx <> C.bsCtx)

C.include "<pty.h>"
C.include "<sys/ioctl.h>"
C.include "<stdio.h>"
C.include "<stdlib.h>"

execWithPty ::
  (Int, Int) ->
  FilePath ->
  [String] ->
  IO
    ( ByteStream IO (),
      ByteStream IO (),
      ByteStream IO () -> IO (),
      IO (),
      Process.ProcessHandle
    )
execWithPty size fp args = do
  wsStdout <- createWinsize size
  wsStderr <- createWinsize size
  wsStdin <- createWinsize size
  (stdoutMaster, stdoutSlave) <- openPty wsStdout
  (stderrMaster, stderrSlave) <- openPty wsStderr
  (stdinMaster, stdinSlave) <- openPty wsStdin
  mapM_ (`hSetBuffering` NoBuffering) [stdoutMaster, stderrMaster, stdinSlave]
  let p =
        (Process.proc fp args)
          { Process.std_out = Process.UseHandle stdoutMaster,
            Process.std_err = Process.UseHandle stderrMaster,
            Process.std_in = Process.UseHandle stdinMaster
          }
  (_, _, _, processHandle) <- Process.createProcess_ fp p

  let closeHandles = do
        mapM_
          hFlush
          [stdoutMaster, stderrMaster, stdinMaster, stdoutSlave, stderrSlave, stdinSlave]
        mapM_
          hClose
          [stdoutMaster, stderrMaster, stdinMaster]
        mapM_
          free
          [wsStdout, wsStderr, wsStdin]

  return
    ( StreamingBS.hGetContents stdoutSlave,
      StreamingBS.hGetContents stderrSlave,
      StreamingBS.toHandle stdinSlave,
      closeHandles,
      processHandle
    )

createWinsize :: (Int, Int) -> IO (Ptr ())
createWinsize (height, width) =
  let cheight = fromIntegral height
      cwidth = fromIntegral width
   in [C.block|
      void* {
        struct winsize* ws = malloc(sizeof(struct winsize));
        ws -> ws_row = $(int cheight);
        ws -> ws_col = $(int cwidth);
        return ws;
      }
      |]

openPty :: Ptr () -> IO (Handle, Handle)
openPty ws = do
  (masterFd, slaveFd) <-
    C.withPtrs_ @(C.CInt, C.CInt) $ \(master_ptr, slave_ptr) -> do
      ret <-
        [C.block|
          int {
            openpty($(int* master_ptr), $(int* slave_ptr), NULL, NULL, $(void* ws));
          }
        |]
      when (ret /= 0) (fail $ "openpty failed with " ++ show ret ++ ".")
  (,)
    <$> fdToHandle (Fd masterFd)
    <*> fdToHandle (Fd slaveFd)
