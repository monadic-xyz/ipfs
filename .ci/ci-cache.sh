#!/usr/bin/env bash

set -euox pipefail

CACHE_BUCKET="gs://multihash-build-cache"

function load-cache() {
  gsutil -m cp "$CACHE_BUCKET/dot-cabal.tar.gz" . || true
  if [[ -f dot-cabal.tar.gz ]]; then
    tar -xzf dot-cabal.tar.gz -C $HOME
  fi
}

function save-cache() {
  tar -czf dot-cabal.tar.gz -C $HOME \
    --exclude=".cabal/bin/git-remote-ipfs" \
    --exclude=".cabal/packages/hackage.haskell.org/build-reports.log" \
    --exclude=".cabal/packages/hackage.haskell.org/00-index*" \
    --exclude=".cabal/packages/hackage.haskell.org/01-index*" \
    --exclude=".cabal/packages/hackage.haskell.org/*.json" \
    --exclude=".cabal/packages/hackage.haskell.org/hackage-security-lock" \
    --exclude=".cabal/logs" \
    .cabal

  gsutil -m cp dot-cabal.tar.gz "$CACHE_BUCKET/"
}
