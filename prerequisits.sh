#!/bin/bash
uname=`uname`

if [ "$uname" == 'Darwin' ]; then
  brew update
  brew upgrade
  brew install autoconf automake pcre gnupg
fi

