#!/bin/bash
uname=`uname`

if [ "$uname" == 'Darwin' ]; then
  brew update
  brew upgrade
  brew install autoconf automake pcre gnupg
fi

# cabal update
# cabal install hello
# cabal install hasktags
# cabal install happy
# cabal install alex
# cabal install hprotoc
# cabal install ghc-mod
# cabal install hoogle
