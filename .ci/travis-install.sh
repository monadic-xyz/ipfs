#!/usr/bin/env bash

set -xeuo pipefail

: ${GHCVER?}
: ${CABALVER?}

travis_retry () {
    $* || (sleep 1 && $*) || (sleep 2 && $*)
}

gpg --version

if ! [ -x $HOME/.ghcup/bin/ghcup ]; then
    mkdir -p $HOME/.ghcup/bin
    cd $HOME/.ghcup/bin
    travis_retry curl -LO https://github.com/haskell/ghcup/releases/download/0.0.7/ghcup
    travis_retry curl -LO https://github.com/haskell/ghcup/releases/download/0.0.7/ghcup.asc
    travis_retry gpg --keyserver keyserver.ubuntu.com --recv-keys 256844E8AE55008AF197C1B7511B62C09D50CD28
    gpg --verify ghcup.asc ghcup
    chmod +x $HOME/.ghcup/bin/ghcup
fi

export PATH="$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH"

ghcup set "$GHCVER" || ghcup install "$GHCVER" && ghcup set "$GHCVER"
ghcup install-cabal "$CABALVER"
