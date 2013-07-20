#!/bin/bash
ORGDIR=`pwd`
cd ..
. environment.sh
cd $ORGDIR

mongo localhost:$mongo_port/base_geo_data buildSearchCollection.js 
