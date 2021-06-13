module Streaming.Attoparsec where

import "attoparsec" Data.Attoparsec.Types
import "streaming" Streaming (Of, Stream)
import "streaming" Streaming.Prelude qualified as Streaming

-- | Repeatedly run an attoparsec parser through stream elements. Any failed parse aborts the stream.
parsed ::
  forall a t m r.
  (Monad m, Eq t, Monoid t) =>
  (t -> IResult t a) ->
  Stream (Of t) m r ->
  Stream (Of a) m (Either ([String], String) r)
parsed parser = go Nothing Nothing
  where
    {-@ lazy go @-}
    go ::
      Maybe (t -> IResult t a) -> -- possible incomplete parse
      Maybe t -> -- data which isn't yet fed into the parser
      Stream (Of t) m r -> -- rest of the stream
      Stream (Of a) m (Either ([String], String) r)
    -- ignore empty chunks
    go f (Just t) stream | t == mempty = go f Nothing stream
    -- if there is no partial parse and no chunk, read the next chunk
    go Nothing Nothing stream =
      lift (Streaming.next stream) >>= \case
        Left r -> return $ Right r
        Right (chunk, rest) -> go Nothing (Just chunk) rest
    -- if there is data, start the parser
    go Nothing (Just chunk) stream =
      go (Just parser) (Just chunk) stream
    -- if there is partial parse but no chunk, read the next chunk
    go (Just f) Nothing stream =
      lift (Streaming.next stream) >>= \case
        Left r -> case f mempty of
          Done _ a -> Streaming.yield a $> Right r
          Fail _ ctx err -> return $ Left (ctx, err)
          Partial _ -> error "parser returned partial after EOF"
        Right (chunk, rest) ->
          go (Just f) (Just chunk) rest
    -- if there is partial parse and a chunk, feed the parser
    go (Just f) (Just chunk) stream =
      case f chunk of
        Done i a -> Streaming.yield a *> go Nothing (Just i) stream
        Fail _ ctx err -> return $ Left (ctx, err)
        Partial f' -> go (Just f') Nothing stream
