#!/bin/bash

ORGDIR=`pwd`
cd ..
. environment.sh
cd $ORGDIR

#Download data
if [ ! -d "$download/geo-data/postcode" ]; then
  mkdir -p $download/geo-data/postcode
  cd $download/geo-data/postcode
  curl -C - -O http://download.geonames.org/export/zip/GB_full.csv.zip
  unzip GB_full.csv.zip
  mongoimport --host localhost:$mongo_port --db base_geo_data --collection PostalCode --drop --type tsv --file $download/geo-data/postcode/GB_full.csv -f countryCode,postalCode,placeName,adminName1,adminCode1,adminName2,adminCode2,adminName3,adminCode3,latitude,longitude,accuracy
  mongo localhost:$mongo_port/base_geo_data --eval "db.PostalCode.ensureIndex({postalCode:1});"
fi

if [ ! -d "$download/geo-data/placename" ]; then
  mkdir -p $download/geo-data/placename
  cd $download/geo-data/placename
  curl -C - -O http://download.geonames.org/export/dump/GB.zip
  unzip GB.zip
  mongoimport --host localhost:$mongo_port --db base_geo_data --collection PlaceName --drop --type tsv --file $download/geo-data/placename/GB.txt -f geonameid,name,asciiname,alternatenames,latitude,longitude,featureClass,featureCode,countryCode,cc2,admin1Code,admin2Code,admin3Code,admin4Code,population,elevation,dem,timezone,modificationDate
  mongo localhost:$mongo_port/base_geo_data --eval "db.PlaceName.ensureIndex({name:1});"
fi

if [ ! -d "$download/geo-data/openstreetmap" ]; then
  mkdir -p $download/geo-data/openstreetmap
  cd $download/geo-data/openstreetmap
  curl -C - -O http://download.geofabrik.de/europe/great-britain/england-latest.osm.pbf
  OSMImport mongo localhost:$mongo_port base_geo_data $download/geo-data/openstreetmap/england-latest.osm.pbf
fi


