module Midye.ANSI.Parser
  ( TermBytes (..),
    TermSpecial (..),
    ControlCharacter (..),
    TermSequenceNonCSI (..),
    TermSequenceCSI (..),
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
  = TSControlCharacter ControlCharacter
  | TSTermSequenceNonCSI TermSequenceNonCSI
  | TSTermSequenceCSI TermSequenceCSI
  deriving stock (Show, Eq)

data ControlCharacter
  = CC_BEL
  | CC_BS
  | CC_HT
  | CC_LF
  | CC_CR
  | CC_SO
  | CC_SI
  | CC_CAN
  | CC_ESC
  | CC_DEL
  | CC_CSI
  | CCUnknown Char
  deriving stock (Show, Eq)

data TermSequenceNonCSI
  = TSNC_RIS
  | TSNC_IND
  | TSNC_NEL
  | TSNC_HTS
  | TSNC_RI
  | TSNC_DECID
  | TSNC_DECSC
  | TSNC_DECRC
  | TSNC_CSI
  | TSNC_OSC
  | TSNCUnknown Char
  deriving stock (Show, Eq)

data TermSequenceCSI
  = TSC_SGR [Int]
  | TSCUnknown Char
  deriving stock (Show, Eq)

pControlCharacter :: Parser ControlCharacter
pControlCharacter = do
  c <- Attoparsec.anyChar
  case ord c of
    0x07 -> return CC_BEL
    0x08 -> return CC_BS
    0x09 -> return CC_HT
    0x0A -> return CC_LF
    0x0D -> return CC_CR
    0x0E -> return CC_SO
    0x0F -> return CC_SI
    0x18 -> return CC_CAN
    0x1B -> return CC_ESC
    0x7F -> return CC_DEL
    0x9B -> return CC_CSI
    _ -> fail "not a control character"

-- Assuming an 'ESC' character is parsed already.
pTermSequenceNonCSI :: Parser TermSequenceNonCSI
pTermSequenceNonCSI =
  Attoparsec.anyChar >>= \case
    'c' -> return TSNC_RIS
    'D' -> return TSNC_IND
    '[' -> return TSNC_CSI
    -- FIXME: There're more
    other -> return $ TSNCUnknown other

-- Assuming an 'ESC [' or 'CSI' sequence is parsed already.
-- FIXME: When parsing sequences, another control character/sequence might start.
pTermSequenceCSI :: Parser TermSequenceCSI
pTermSequenceCSI = do
  params <- Attoparsec.decimal `Attoparsec.sepBy` Attoparsec.char ';'
  action <- Attoparsec.anyChar
  case action of
    'm' -> return $ TSC_SGR params
    -- FIXME: There're more
    other -> return $ TSCUnknown other

pTermSpecial :: Parser TermSpecial
pTermSpecial = do
  cc <- pControlCharacter
  if cc == CC_ESC
    then
      ( do
          ncsi <- pTermSequenceNonCSI
          if ncsi == TSNC_CSI
            then TSTermSequenceCSI <$> pTermSequenceCSI
            else return $ TSTermSequenceNonCSI ncsi
      )
        <|> (return $ TSControlCharacter cc)
    else
      if cc == CC_CSI
        then TSTermSequenceCSI <$> pTermSequenceCSI
        else return $ TSControlCharacter cc

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
