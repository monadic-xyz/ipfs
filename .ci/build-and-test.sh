#!/usr/bin/env bash

set -euxo pipefail

apt-get update
DEBIAN_FRONTEND=noninteractive \
  apt-get install -y --no-install-recommends libssl-dev libicu-dev

export PATH="/builder/home/.local/bin:$PATH"
stack build \
  --test \
  --no-terminal \
  --pedantic \
  --flag git-remote-ipfs:with-e2e-tests
