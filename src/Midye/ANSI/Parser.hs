module Midye.ANSI.Parser
  ( run,
  )
where

import "attoparsec" Data.Attoparsec.Text (Parser)
import "attoparsec" Data.Attoparsec.Text qualified as Attoparsec
import "this" Midye.ANSI.Types
import "streaming" Streaming (Of, Stream)
import "this" Streaming.Attoparsec (parsed)
import "streaming-bytestring" Streaming.ByteString qualified as StreamingBS
import "this" Streaming.Text qualified

pTermSpecial :: Parser TermSpecial
pTermSpecial = do
  c <- Attoparsec.anyChar
  case ord c of
    0x07 -> return TS_BEL
    0x08 -> return TS_BS
    0x09 -> return TS_HT
    0x0A -> return TS_LF
    0x0D -> return TS_CR
    0x0E -> return TS_LS0
    0x0F -> return TS_LS1
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
    '(' -> TS_SCS0 <$> Attoparsec.anyChar
    ')' -> TS_SCS1 <$> Attoparsec.anyChar
    '*' -> TS_SCS2 <$> Attoparsec.anyChar
    '+' -> TS_SCS3 <$> Attoparsec.anyChar
    '-' -> TS_SCS1 <$> Attoparsec.anyChar
    '.' -> TS_SCS2 <$> Attoparsec.anyChar
    '/' -> TS_SCS3 <$> Attoparsec.anyChar
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
      case (xtermExt, action, params) of
        (False, 'm', ps) -> return $ TS_SGR ps
        (False, 'r', ps) -> return $ TS_DECSTBM ps
        (False, 'H', [row]) -> return $ TS_CUP row 0
        (False, 'H', [row, col]) -> return $ TS_CUP row col
        (False, 'J', ps) -> return $ TS_ED ps
        (False, 'K', [0]) -> return $ TS_EL EraseInLineToEnd
        (False, 'K', [1]) -> return $ TS_EL EraseInLineToBeginning
        (False, 'K', [2]) -> return $ TS_EL EraseInLineAll
        (False, 'A', [c]) -> return $ TS_CUU c
        (False, 'B', [c]) -> return $ TS_CUD c
        (False, 'C', [c]) -> return $ TS_CUF c
        (False, 'D', [c]) -> return $ TS_CUB c
        (False, 'G', [col]) -> return $ TS_CHA col
        (False, 'd', [row]) -> return $ TS_LPA row
        -- TODO: There're more
        (ext, other, ps) -> return $ TSUnknown (TSUnknownCSISequence ext other ps)

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
