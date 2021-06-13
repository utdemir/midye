module Tests.Streaming.Text where

import "bytestring" Data.ByteString.Lazy qualified as LByteString
import "text" Data.Text.Encoding.Error qualified as Text
import "text" Data.Text.Lazy qualified as LText
import "text" Data.Text.Lazy.Encoding qualified as LText
import "hedgehog" Hedgehog
import "hedgehog" Hedgehog.Gen qualified as Gen
import "hedgehog" Hedgehog.Range qualified as Range
import "streaming-bytestring" Streaming.ByteString qualified as StreamingBS
import "streaming" Streaming.Prelude qualified as Streaming
import "this" Streaming.Text qualified
import TestArgs

hprop_decodeUtf8 :: Property
hprop_decodeUtf8 = fastPropertyTest . property $ do
  inputChunks <- forAll $ Gen.list (Range.linear 0 100) (Gen.bytes (Range.linear 1 20))
  let input = LByteString.fromChunks inputChunks

  let expected =
        input
          & LText.decodeUtf8With Text.lenientDecode
          & LText.toStrict

  let actual =
        Streaming.each @Identity inputChunks
          & StreamingBS.fromChunks
          & Streaming.Text.decodeUtf8
          & Streaming.mconcat
          & runIdentity
          & Streaming.fst'

  expected === actual
