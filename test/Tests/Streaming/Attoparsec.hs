module Tests.Streaming.Attoparsec where

import "attoparsec" Data.Attoparsec.Text qualified as Attoparsec
import "hedgehog" Hedgehog
import "hedgehog" Hedgehog.Gen qualified as Gen
import "hedgehog" Hedgehog.Range qualified as Range
import "this" Streaming.Attoparsec (parsed)
import "streaming" Streaming.Prelude qualified as Streaming

exampleParser :: Attoparsec.Parser Text
exampleParser =
  Attoparsec.count 3 Attoparsec.letter
    <&> toText

hprop_parsed :: Property
hprop_parsed = withTests 1000 . property $ do
  inputChunks <- forAll $ Gen.list (Range.linear 0 100) (Gen.text (Range.linear 1 20) Gen.lower)
  let input = mconcat inputChunks

  let expectedOrig = Attoparsec.parseOnly (many exampleParser <* Attoparsec.endOfInput) input
  let expected = expectedOrig & either (const Nothing) Just
  let actualOrig =
        Streaming.each @Identity inputChunks
          & parsed (Attoparsec.parse exampleParser)
          & Streaming.toList
  let Identity (xs Streaming.:> r) = actualOrig
  let actual = either (const Nothing) (const (Just xs)) r

  annotateShow expectedOrig

  annotateShow actualOrig

  expected === actual
