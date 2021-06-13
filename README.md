# Midye

A command-line shell with superpowers. WIP.

Every command is provided their own PTY, which then can be rendered to the screen, saved or manipulated independently.

## Features

### Implemented

* Run a command with virtual std{err,stdout,in}.
* Parse the output for (some) escape sequences, and render them on a virtual framebuffer.
  * Randomized-testing against `tmux`, so we at least know that the escape codes work sanely.

### WIP

* Allow interacting with the running program. Show the live output, pass the user input.
* Complete-enough support for escape sequences.
* REPL

### Future

* Time-travelling scrollback
* Highlight stderr/stdout differently
* Bash-like command language
* Expand/collapse previous command output
* Use previous commands output on the next command
* History (both the command and outputs)
* Integrated `pv`
* Extension support (likely using `lua`)

## Developer docs

### Console escape sequences

* Linux console escape and control sequences: https://man7.org/linux/man-pages/man4/console_codes.4.html
* Summary of ANSI standards for ASCII terminals: https://www.inwap.com/pdp10/ansicode.txt
* hTerm control sequences: https://chromium.googlesource.com/apps/libapps/+/a5fb83c190aa9d74f4a9bca233dac6be2664e9e9/hterm/doc/ControlSequences.md
