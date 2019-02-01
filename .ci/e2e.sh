#!/usr/bin/env bash
set -euxo pipefail

export PATH="/builder/home/.cabal/bin:$PATH"
cabal v2-install git-remote-ipfs
cabal v2-test -fwith-e2e-tests git-remote-ipfs
