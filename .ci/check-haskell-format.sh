#!/usr/bin/env bash

set -euo pipefail

shopt -s globstar

PATH=$HOME/.local/bin:$PATH

base=$(mktemp -d "/tmp/gossip-base.XXXXX")
srcs=$(find . -name "*.hs" \
              -not -name "Setup.hs" \
              -not -name "Paths_*.hs" \
              -not -path "dist*" \
              -not -path "*/gen/*"\
              -not -path "*.stack*")

for f in $srcs; do
  path=$base/$(dirname "$f")
  mkdir -p "$path"
  cp "$f" "$path"
done

formatted=$(mktemp -d "/tmp/gossip-formatted.XXXXX")
cp -R "$base"/* "$formatted"

stylish-haskell "$formatted"/**/**.hs --inplace

diff -rq "$base" "$formatted"
