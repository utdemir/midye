module Tests.Midye.ANSI.Parser where

import "resourcet" Control.Monad.Trans.Resource
import Data.List (span)
import "text" Data.Text.Lazy.Builder qualified as Builder
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
    pp inp =
      cc inp
        & map
          ( \case
              Parser.TBPlain i -> encodeUtf8 (Builder.toLazyText i)
              other -> show other
          )
        & map (<> "\n")
        & mconcat
      where
        cc :: [Parser.TermBytes] -> [Parser.TermBytes]
        cc (Parser.TBSpecial o : xs) = Parser.TBSpecial o : cc xs
        cc ((Parser.TBPlain b) : xs) =
          let (ps, rest) =
                span
                  ( \case
                      Parser.TBPlain _ -> True
                      _ -> False
                  )
                  xs
              ps' = map (\(Parser.TBPlain i) -> i) ps
           in Parser.TBPlain (mconcat (b : ps')) : cc rest
        cc [] = []
