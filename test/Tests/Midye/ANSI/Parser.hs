module Tests.Midye.ANSI.Parser where

import "resourcet" Control.Monad.Trans.Resource
import "text" Data.Text qualified as Text
import "this" Midye.ANSI.Parser qualified as Parser
import "streaming-bytestring" Streaming.ByteString qualified as StreamingBS
import "streaming" Streaming.Prelude qualified as Streaming
import "filepath" System.FilePath (takeBaseName)
import "tasty" Test.Tasty
import "tasty-golden" Test.Tasty.Golden (findByExtension, goldenVsStringDiff)

test_golden :: IO [TestTree]
test_golden = do
  ansiFiles <- findByExtension [".ansi"] "./test/data/"
  return $
    [ goldenVsStringDiff
        (takeBaseName ansiFile)
        (\ref new -> ["diff", "--color=always", "-u", ref, new])
        retFile
        ( runResourceT $
            StreamingBS.readFile @(ResourceT IO) ansiFile
              & Parser.run
              & Streaming.toList
              <&> pp . Streaming.fst'
        )
      | ansiFile <- ansiFiles,
        let retFile = ansiFile ++ ".out"
    ]
  where
    pp :: [Parser.TermBytes] -> LByteString
    pp [] = "\n"
    pp (Parser.TBPlain i : Parser.TBPlain j : xs) =
      encodeUtf8 (Text.singleton i) <> pp (Parser.TBPlain j : xs)
    pp (Parser.TBPlain i : xs) =
      encodeUtf8 (Text.singleton i) <> "\n" <> pp xs
    pp (Parser.TBSpecial i : xs) =
      encodeUtf8 (show @Text i) <> "\n" <> pp xs
