module Midye.ANSI.Parser
  ( TermBytes (..),
    TermSpecial (..),
    run,
  )
where

import "attoparsec" Data.Attoparsec.Text (Parser)
import "attoparsec" Data.Attoparsec.Text qualified as Attoparsec
import "streaming" Streaming (Of, Stream)
import "this" Streaming.Attoparsec (parsed)
import "streaming-bytestring" Streaming.ByteString qualified as StreamingBS
import "this" Streaming.Text qualified

data TermBytes
  = TBPlain Char
  | TBSpecial TermSpecial
  deriving stock (Show, Eq)

data TermSpecial
  = -- control chars
    TS_BEL
  | TS_BS
  | TS_HT
  | TS_LF
  | TS_CR
  | TS_SO
  | TS_SI
  | TS_CAN
  | TS_ESC
  | TS_DEL
  | TS_CSI
  | -- Non CSI sequences
    TS_RIS
  | TS_IND
  | TS_NEL
  | TS_HTS
  | TS_RI
  | TS_DECPNM
  | TS_DECPAM
  | -- CSI sequences
    TS_SGR [Int]
  | TS_DECSTBM [Int]
  | TS_DECCKM Bool
  | TS_DECTCEM Bool
  | TS_CUP [Int]
  | TS_CUU [Int]
  | TS_CUD [Int]
  | TS_CUF [Int]
  | TS_CUB [Int]
  | TS_ED [Int]
  | TS_EL [Int]
  | TS_StartBlinkingCursor Bool
  | TS_X11MouseReporting Bool
  | TS_BracketedPasteMode Bool
  | -- unknown
    TSUnknown TSUnknown
  deriving stock (Show, Eq)

data TSUnknown
  = TSUnknownCSISequence Bool Char [Int]
  | TSUnknownDECSequence Integer Bool
  deriving stock (Show, Eq)

pTermSpecial :: Parser TermSpecial
pTermSpecial = do
  c <- Attoparsec.anyChar
  case ord c of
    0x07 -> return TS_BEL
    0x08 -> return TS_BS
    0x09 -> return TS_HT
    0x0A -> return TS_LF
    0x0D -> return TS_CR
    0x0E -> return TS_SO
    0x0F -> return TS_SI
    0x18 -> return TS_CAN
    0x1B ->
      pTermSequenceNonCSI
        <|> return TS_ESC
    0x7F -> return TS_DEL
    0x9B ->
      pTermSequenceCSI
        <|> return TS_CSI
    _ -> fail "not a control character"

-- Assuming an 'ESC' is parsed already.
pTermSequenceNonCSI :: Parser TermSpecial
pTermSequenceNonCSI = do
  Attoparsec.anyChar >>= \case
    'c' -> return TS_RIS
    'D' -> return TS_IND
    '[' -> pTermSequenceCSI
    '>' -> return TS_DECPNM
    '=' -> return TS_DECPAM
    -- TODO: There're more
    _ -> fail "unknown non-csi sequence"

-- Assuming an 'ESC [' or 'CSI' sequence is parsed already.
pTermSequenceCSI :: Parser TermSpecial
pTermSequenceCSI = decExtensions <|> actualCSI
  where
    actualCSI :: Parser TermSpecial
    actualCSI = do
      xtermExt <- (Attoparsec.char '>' $> True) <|> return False
      -- FIXME: should be at most 16 times. possible DOS without this limit.
      params <- (Attoparsec.decimal <|> return 0) `Attoparsec.sepBy` Attoparsec.char ';'
      action <- Attoparsec.anyChar
      case (xtermExt, action) of
        (False, 'm') -> return $ TS_SGR params
        (False, 'r') -> return $ TS_DECSTBM params
        (False, 'H') -> return $ TS_CUP params
        (False, 'J') -> return $ TS_ED params
        (False, 'K') -> return $ TS_EL params
        (False, 'A') -> return $ TS_CUU params
        (False, 'B') -> return $ TS_CUD params
        (False, 'C') -> return $ TS_CUF params
        (False, 'D') -> return $ TS_CUB params
        -- TODO: There're more
        (ext, other) -> return $ TSUnknown (TSUnknownCSISequence ext other params)

    decExtensions :: Parser TermSpecial
    decExtensions = do
      _ <- Attoparsec.char '?'
      n <- Attoparsec.decimal @Integer
      mode <-
        Attoparsec.char 'h' $> True
          <|> Attoparsec.char 'l' $> False
      case n of
        1 -> return $ TS_DECCKM mode
        12 -> return $ TS_StartBlinkingCursor mode
        25 -> return $ TS_DECTCEM mode
        1000 -> return $ TS_X11MouseReporting mode
        2004 -> return $ TS_BracketedPasteMode mode
        -- TODO: There're more
        _ -> return $ TSUnknown (TSUnknownDECSequence n mode)

pTermBytes :: Parser TermBytes
pTermBytes = do
  (TBSpecial <$> pTermSpecial)
    <|> (Attoparsec.anyChar <&> TBPlain)

run :: Monad m => StreamingBS.ByteStream m a -> Stream (Of TermBytes) m a
run bs =
  bs
    & Streaming.Text.decodeUtf8
    & parsed (Attoparsec.parse pTermBytes)
    <&> fromRight (error "invariant violation: pTermBytes finished")
