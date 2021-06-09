module Tests.Midye.ANSI where

import "bytestring" Data.ByteString qualified as ByteString
import Data.List qualified
import "hedgehog" Hedgehog
import "hedgehog" Hedgehog.Gen qualified as Gen
import "hedgehog" Hedgehog.Range qualified as Range
import "this" Midye.ANSI.Parser qualified
import "this" Midye.ANSI.Printer
import Numeric (showHex)
import "streaming-bytestring" Streaming.ByteString qualified as StreamingBS
import "streaming" Streaming.Prelude qualified as Streaming
import "filepath" System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import "process" System.Process
import "HUnit" Test.HUnit.Base

-- | Sanity check for tmuxCapture.
unit_tmuxCapture :: IO ()
unit_tmuxCapture = do
  ret <- tmuxCapture (10, 20) "foobar"
  ("foobar" ++ replicate 10 '\n') @=? stripTrailingSpaces ret

hprop_tmuxRefSimple :: Property
hprop_tmuxRefSimple = tmuxRefWithChars [0x61, 0x62, 0x63]

hprop_tmuxRefCRLF :: Property
hprop_tmuxRefCRLF = tmuxRefWithChars [0x61, 0x62, 0x0a, 0x0d]

hprop_tmuxRefHorizontalTab :: Property
hprop_tmuxRefHorizontalTab = tmuxRefWithChars [0x61, 0x62, 0x09]

-- * Utils

tmuxRefWithChars :: [Word8] -> Property
tmuxRefWithChars chrs = withTests 40 . withShrinks 40 . property $ do
  annotateShow chrs
  input <- forAll $ Gen.list (Range.linear 0 500) (Gen.element chrs)
  let size = (10, 20)

  expected <- evalIO $ tmuxCapture size (ByteString.pack input)

  let actual =
        input
          & Streaming.each
          & StreamingBS.pack
          & Midye.ANSI.Parser.run
          & Streaming.fold (flip Midye.ANSI.Printer.run) (Midye.ANSI.Printer.initVTY size) id
          & runIdentity
          & Streaming.fst'
          & view Midye.ANSI.Printer.vtyScreen
          & fmap (map (view cellContent) . toList)
          & toList
          & concatMap (\i -> i ++ ['\n'])

  stripTrailingSpaces expected === stripTrailingSpaces actual

-- | Cat the contents of the filepath on a terminal with given size, and return the screen contents
tmuxCapture :: (Int, Int) -> ByteString -> IO String
tmuxCapture (height, width) input = do
  withSystemTempDirectory "midye-tests-tmux" $ \dir -> do
    let tmuxArgs = ["-S", dir </> "tmux.sock", "-f", "/dev/null"]
        callTmux args = callProcess "tmux" (tmuxArgs ++ args)
        readTmux args = readProcess "tmux" (tmuxArgs ++ args) ""
        encoded =
          input
            & ByteString.unpack
            & concatMap (\w8 -> "\\x" ++ showHex w8 "")
    callTmux $
      ["new-session", "-d", "-x", show width, "-y", show height]
        ++ ["echo -ne '" ++ encoded ++ "'; tmux wait-for -S finished; sleep 5"]
    callTmux ["wait-for", "finished"]
    out <- readTmux ["capture-pane", "-p", "-N"]
    callTmux ["kill-session"]
    return out

stripTrailingSpaces :: String -> String
stripTrailingSpaces =
  Data.List.unlines
    . map (dropWhileEnd (== ' '))
    . Data.List.lines
  where
    dropWhileEnd :: (a -> Bool) -> [a] -> [a]
    dropWhileEnd p = reverse . dropWhile p . reverse
