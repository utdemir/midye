{-# LANGUAGE CPP #-}

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
import System.Timeout (timeout)
import "tasty" Test.Tasty
import "tasty-hedgehog" Test.Tasty.Hedgehog (testProperty)
import "this" TestArgs

-- | Sanity check for tmuxCapture.
hprop_tmuxCapture :: Property
hprop_tmuxCapture = withTests 1 . property $ do
  ret <- evalIO $ tmuxCapture (10, 20) "foobar"
  ("foobar" ++ replicate 10 '\n') === stripTrailingSpaces ret

test_tmuxRefProp :: [TestTree]
test_tmuxRefProp =
  [ testProperty
      ("single/" ++ name)
      (tmuxRefWithChars (Gen.choice [simpleChars, code]))
    | (name, code) <- controlChars
  ]
    ++ [ testProperty
           "control chars"
           (tmuxRefWithChars $ Gen.choice (simpleChars : map snd controlChars))
       ]
  where
    simpleChars :: Gen [Word8]
    simpleChars = Gen.element [[0x61], [0x62]]
    controlChars :: [(String, Gen [Word8])]
    controlChars =
      [ ("backspace", return [0x08]),
        ("carriage return", return [0x0d]),
        ("horizontal tab", return [0x09]),
        ("newline", return [0x0a]),
        ( "cursor up/down/forward/backward",
          do
            dir <- Gen.element [0x41, 0x42, 0x43, 0x44]
            return [0x1b, 0x5b, 0x31, dir]
        )
      ]

test_tmuxRefUnit :: [TestTree]
test_tmuxRefUnit =
  [ testProperty
      (hexify (ByteString.unpack bs))
      (withTests 1 . property $ tmuxRefUnit bs)
    | bs <- cases
  ]
  where
    cases =
      [ "aa\t\bc",
        "a\n\bc",
        "\x9\x9\x9\x61\x1b\x5b\x31\x41\x61"
      ]

test_tmuxRefData :: [TestTree]
test_tmuxRefData =
  [ testProperty fpath . withTests 1 . property $ do
      bs <- evalIO $ ByteString.readFile fpath
      tmuxRefUnit bs
    | fpath <- files
  ]
  where
    files =
      [ "test/data/hexyl.ansi"
      ]

-- * Utils

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
          & Streaming.fold (flip Midye.ANSI.Printer.run) (Midye.ANSI.Printer.initVTY size) id
          & runIdentity
          & Streaming.fst'
          & view Midye.ANSI.Printer.vtyScreen
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
