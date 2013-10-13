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

build_bin=$base/dist/build/OSMImport

export PATH=$build_bin:$haskell_bin:$PATH
