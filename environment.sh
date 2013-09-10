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

#MongoDB
mongo_install=$platform/mongo
mongo_data=$platform/data/mongo
mongo_bin=$platform/mongo/bin
mongo_port=7720
mongo_version=2.4.6

#Redis
redis=$platform/redis
redis_version=2.6.16
redis_bin=$redis
redis_port=7721
redis_data=$platform/data/redis

export PATH=$haskell_bin:$mongo_bin:$redis_bin:$PATH

