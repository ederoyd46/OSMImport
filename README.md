Description
-----------

This application parses data from the Open Street Map Protocol Buffer Format (http://wiki.openstreetmap.org/wiki/PBF_Format) and imports it into a MongoDB database.

It has been written in Haskell and was inspired by the https://github.com/larroy/osmcompiler project.

Build Instructions
------------------

1. Run `stack build``

Usage
-----

Three options are needed to start the import process

1. dbconnection - the host and port the database is running on (authentication is currently not supported).
2. dbname - the name of the database you want to import into.
3. filename - the name of the file to import.


[EXAMPLE]

OSMImport '127.0.0.1:7720' 'geo_data' './download/england-latest.osm.pbf'


Docker Usage
------------
Pull down the repository

```
docker pull ederoyd46/osmimport
```

Run an import, assumes you have a container called mongo, and have downloaded the england data from OSM in protocol buffer format

```
docker run -d --name mongo -p 27017:27017 -v $(pwd)/data:/data/db mongo
docker run -it --rm=true --link mongo:mongo -v $(pwd)/download:/data ederoyd46/osmimport 'mongo:27017' 'geo_data' '/data/england-latest.osm.pbf'
```

