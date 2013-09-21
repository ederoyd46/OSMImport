#!/bin/bash

#Initial config
base=`pwd`
uname=`uname`
platform=$base/platform
platform_src=$platform/source
config=$base/config
log_dir=$platform/logs
download=$base/download
process_owner=`whoami`

#Haskell
haskell_install=$platform/osmimport
haskell_bin=$haskell_install/bin

export PATH=$haskell_bin:$PATH

