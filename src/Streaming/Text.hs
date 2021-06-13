module Streaming.Text where

import "bytestring" Data.ByteString qualified as ByteString
import "text" Data.Text.Encoding qualified as Text
import "text" Data.Text.Encoding.Error qualified as Text
import "streaming" Streaming (Of, Stream)
import "streaming-bytestring" Streaming.ByteString (ByteStream)
import "streaming-bytestring" Streaming.ByteString qualified as StreamingBS
import "streaming" Streaming.Prelude qualified as Streaming

decodeUtf8 :: forall m r. Monad m => ByteStream m r -> Stream (Of Text) m r
decodeUtf8 inp =
  inp
    & StreamingBS.toChunks
    & Streaming.filter (not . ByteString.null)
    & go (Text.streamDecodeUtf8With Text.lenientDecode "")
  where
    {-@ lazy go @-}
    go ::
      Text.Decoding -> -- current state
      Stream (Of ByteString) m r -> -- rest of the stream
      Stream (Of Text) m r
    go (Text.Some parsed leftover cb) stream =
      case parsed of
        "" ->
          lift (Streaming.next stream) >>= \case
            Left r -> do
              unless (ByteString.null leftover) $
                Streaming.yield "\xfffd"
              return r
            Right (chunk, rest) ->
              go (cb chunk) rest
        res -> do
          () <- Streaming.yield res
          go (Text.Some "" leftover cb) stream
