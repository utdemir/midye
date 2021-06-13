#!/usr/bin/env bash
ghcid -c 'cabal repl tests -f fast-tests --repl-options=+RTS --repl-options=-N --repl-options=-RTS' --restart midye.cabal $@ -T main
