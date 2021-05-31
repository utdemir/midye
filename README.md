# Midye

## Developer docs

* Linux console escape and control sequences: https://man7.org/linux/man-pages/man4/console_codes.4.html
* Summary of ANSI standards for ASCII terminals: https://www.inwap.com/pdp10/ansicode.txt
* hTerm control sequences: https://chromium.googlesource.com/apps/libapps/+/a5fb83c190aa9d74f4a9bca233dac6be2664e9e9/hterm/doc/ControlSequences.md
## Ideas

* Onceki komutlarin stderr ve stdout'larini sonraki komutlarda kullanabileyim
* Expand/collapse per command
* Calistirilmis process'lerin pid'lerini gorebileyim
  * Resource kullanimlari, acik dosyalar vs.
* Integrated `pv`:
  * Pipe'lerden gecen veri miktarini gorebilmek, progress bar veya tahmini sure bilgisi koymak
  * Pipe'den gecen veriyi kucuk bir pencerede sample'layabilmek
* Komutlari structured editleyebilmek
  * Ctrl-left/right argumanlarda ilerliyor
  * Butun argumani secip tirnak icine falan alabilmek
