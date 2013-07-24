#!/bin/bash
uname=`uname`

if [ "$uname" == 'Darwin' ]; then
  brew update
  brew upgrade
  brew install autoconf automake pcre gnupg haskell-platform
fi

cabal update
#cabal install cabal
#cabal install cabal-install
cabal install hello
cabal install hasktags
cabal install happy
cabal install alex
cabal install hprotoc

