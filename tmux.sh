#!/usr/bin/env sh

set -o errexit

tmpdir="$(mktemp -d)"
trap "rm -rf '$tmpdir'" EXIT

cmd="$1"

TMUX="tmux -S "$tmpdir/tmux.sock" -f /dev/null"
$TMUX new-session -d -x 90 -y 20 "$cmd; tmux wait-for -S finished; sleep 1h"
$TMUX wait-for finished
$TMUX capture-pane -p
$TMUX kill-session
