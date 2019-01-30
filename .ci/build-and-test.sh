#!/usr/bin/env bash

set -euxo pipefail

export PATH="/builder/home/.local/bin:$PATH"
stack build \
  --test \
  --no-terminal \
  --pedantic \
  --flag git-remote-ipfs:with-e2e-tests
