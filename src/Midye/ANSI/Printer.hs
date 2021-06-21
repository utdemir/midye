module Midye.ANSI.Printer where

{-
import Midye.ANSI.Types
import qualified Data.Text as Text

showTB :: TermBytes -> Text
showTB (TBPlain c) = Text.singleton c
showTB (TBSpecial c) = case c of
  TS_BEL -> s 0x07
  TS_BS -> s 0x08
  TS_HT -> s 0x09
  TS_LF -> s 0x0A
  TS_CR -> s 0x0D
  TS_LS0 -> s 0x0A
  TS_LS1 -> s 0x0F
  TS_CAN -> s 0x18
  TS_ESC -> s 0x1B
  TS_DEL -> s 0x7F
  TS_CSI -> s 0x9B
  TS_RIS -> esc <> "c"
  TS_IND -> esc <> "D"
  TS_NEL -> undefined
  TS_HTS -> esc <> ">"
  TS_RI -> undefined
  TS_DECPNM -> esc <> ">"
  TS_DECPAM -> esc <> "="
  (TS_SCS0 c2) -> esc <> "(" <> Text.singleton c2
  (TS_SCS1 c2) -> esc <> ")" <> Text.singleton c2
  (TS_SCS2 c2) -> esc <> "*" <> Text.singleton c2
  (TS_SCS3 c2) -> esc <> "+" <> Text.singleton c2
  (TS_SGR xs) -> esc <> "[" <> Text.intercalate ";" (map show xs) <> "m"
  (TS_DECSTBM xs) -> esc <> "[" <> Text.intercalate ";" (map show xs) <> "r"
  (TS_DECCKM b) -> _
  (TS_DECTCEM b) -> _
  (TS_CUP i i3) -> _
  (TS_CUU i) -> _
  (TS_CUD i) -> _
  (TS_CUF i) -> _
  (TS_CUB i) -> _
  (TS_CHA i) -> _
  (TS_LPA i) -> _
  (TS_ED l_i) -> _
  (TS_EL e) -> _
  (TS_StartBlinkingCursor b) -> _
  (TS_X11MouseReporting b) -> _
  (TS_BracketedPasteMode b) -> _
  (TSUnknown t) -> _
 where
  s = Text.singleton . chr
  esc = Text.singleton (chr 0x1B)
-}
