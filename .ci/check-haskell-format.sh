#!/usr/bin/env bash

set -euo pipefail

shopt -s globstar

PATH=$HOME/.local/bin:$PATH

base=$(mktemp -d "/tmp/gossip-base.XXXXX")
for f in **/**.hs; do
  path=$base/$(dirname "$f")
  mkdir -p "$path"
  cp "$f" "$path"
done

formatted=$(mktemp -d "/tmp/gossip-formatted.XXXXX")
cp -R "$base"/* "$formatted"

stylish-haskell "$formatted"/**/**.hs --inplace

diff -rq "$base" "$formatted"
