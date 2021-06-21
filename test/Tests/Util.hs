{-# LANGUAGE CPP #-}

module Tests.Util where

import "bytestring" Data.ByteString qualified as ByteString
import Data.List qualified
import "hedgehog" Hedgehog
import Numeric (showHex)
import "filepath" System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import "process" System.Process
import System.Timeout (timeout)

-- | Render the given bytestring in tmux, and return the screen contents
tmuxCapture :: (Int, Int) -> ByteString -> IO String
tmuxCapture (height, width) input = do
  withSystemTempDirectory "midye-tests-tmux" $ \dir -> do
    let tmuxArgs = ["-S", dir </> "tmux.sock", "-f", "/dev/null"]
        callTmux args = callProcess "tmux" (tmuxArgs ++ args)
        readTmux args = readProcess "tmux" (tmuxArgs ++ args) ""
        encoded = hexify (ByteString.unpack input)
    Just ret <- timeout 10_000_000 $ do
      callTmux $
        ["new-session", "-d", "-x", show width, "-y", show height]
          ++ ["echo -ne '" ++ encoded ++ "'; tmux wait-for -S finished; sleep 5"]
      callTmux ["wait-for", "finished"]
      out <- readTmux ["capture-pane", "-p", "-N"]
      callTmux ["kill-session"]
      return out
    return $ stripTrailingSpaces ret

-- | Sanity check for tmuxCapture.
hprop_tmuxCapture :: Property
hprop_tmuxCapture = withTests 1 . property $ do
  ret <- evalIO $ tmuxCapture (10, 20) "foobar"
  "foobar" === ret

stripTrailingSpaces :: String -> String
stripTrailingSpaces =
  dropWhileEnd (== '\n')
    . Data.List.unlines
    . map (dropWhileEnd (== ' '))
    . Data.List.lines
  where
    dropWhileEnd :: (a -> Bool) -> [a] -> [a]
    dropWhileEnd p = reverse . dropWhile p . reverse

hexify :: [Word8] -> String
hexify = concatMap (\c -> "\\x" ++ showHex c "")
