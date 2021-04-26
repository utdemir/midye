#!/usr/bin/env bash
ghcid -c 'cabal repl tests' -T main --restart midye.cabal $@
