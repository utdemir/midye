{-# LANGUAGE CPP #-}

module Tests.Midye.ANSI.Render where

import "bytestring" Data.ByteString qualified as ByteString
import "bytestring" Data.ByteString.Char8 qualified as ByteString.Char8
import Data.List qualified
import "hedgehog" Hedgehog
import "hedgehog" Hedgehog.Gen qualified as Gen
import "hedgehog" Hedgehog.Range qualified as Range
import "this" Midye.ANSI.Parser qualified
import "this" Midye.ANSI.Render
import "this" Midye.ANSI.Types
import "this" Midye.ANSI.Widget
import Numeric (showHex)
import "streaming-bytestring" Streaming.ByteString qualified as StreamingBS
import "streaming" Streaming.Prelude qualified as Streaming
import "filepath" System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import "process" System.Process
import System.Timeout (timeout)
import "this" TestArgs

hprop_renderDiff :: Property
hprop_renderDiff = fastPropertyTest . property $ do
  let size = (10, 20)

  preChrs <- forAll $ ByteString.Char8.pack <$> Gen.list (Range.linear 1 100) (Gen.element ['a', ' ', 'b'])
  postChrs <- forAll $ ByteString.Char8.pack <$> Gen.list (Range.linear 1 100) (Gen.element ['a', ' ', 'b'])

  let preVty = feedVTY (initVTY size) preChrs
      postVty = feedVTY (initVTY size) postChrs

      calculatedDiff = renderWidget (Just $ renderVTY preVty) (renderVTY postVty)
      applied = feedVTY preVty calculatedDiff

  showVTY postVty === showVTY applied

-- * Utils

feedVTY :: VTY -> ByteString -> VTY
feedVTY vty bs =
  StreamingBS.fromStrict bs
    & Midye.ANSI.Parser.run
    & Streaming.fold (flip Midye.ANSI.Render.run) vty id
    & runIdentity
    & Streaming.fst'

showVTY :: VTY -> String
showVTY vty =
  vty
    & view Midye.ANSI.Render.vtyScreen
    & fmap (map (view cellContent) . toList . view rowCells)
    & toList
    & concatMap (\i -> i ++ ['\n'])

tmuxRefWithChars :: Gen [Word8] -> Property
tmuxRefWithChars genchr = slowPropertyTest . property $ do
  input <-
    forAll $
      ByteString.pack . concat
        <$> Gen.list (Range.linear 0 100) genchr

  tmuxRefUnit input

tmuxRefUnit :: ByteString -> PropertyT IO ()
tmuxRefUnit input = do
  let size = (10, 20)

  annotate $ hexify (ByteString.unpack input)

  expected <- evalIO $ tmuxCapture size input

  let parsed =
        runIdentity $
          input
            & StreamingBS.fromStrict
            & Midye.ANSI.Parser.run
            & Streaming.toList_
  annotateShow parsed

  let actual =
        Streaming.each parsed
          & Streaming.fold (flip Midye.ANSI.Render.run) (Midye.ANSI.Render.initVTY size) id
          & runIdentity
          & Streaming.fst'
          & view Midye.ANSI.Render.vtyScreen
          & fmap (map (view cellContent) . toList . view rowCells)
          & toList
          & concatMap (\i -> i ++ ['\n'])

  stripTrailingSpaces expected === stripTrailingSpaces actual

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
    return ret

stripTrailingSpaces :: String -> String
stripTrailingSpaces =
  Data.List.unlines
    . map (dropWhileEnd (== ' '))
    . Data.List.lines
  where
    dropWhileEnd :: (a -> Bool) -> [a] -> [a]
    dropWhileEnd p = reverse . dropWhile p . reverse

hexify :: [Word8] -> String
hexify = concatMap (\c -> "\\x" ++ showHex c "")
