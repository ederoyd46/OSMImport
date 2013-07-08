Description
-----------

This application parses data from the Open Street Map Protocol Buffer Format (http://wiki.openstreetmap.org/wiki/PBF_Format) and imports it into a MongoDB database.

It has been written in Haskell and was inspired by the https://github.com/larroy/osmcompiler project.

Current Status
--------------

0.1.0.0 - Initial version, imports (some) node data only. Lots of work to do :)

0.2.0.0 - Added missing node data, now commits to MongoDB in larger batches.

0.3.0.0 - Added support to import into redis, lots of performance improvements.

Installation Instructions
-------------------------

Yes this has been written on a MAC! Although most scripts will just work with Linux, the services aren't yet generated for Linux and I've not added any prerequisits for Linux.

[OSX Users]

1. If you have homebrew (http://brew.sh/) installed then run the prerequisits.sh to install required libraries.

[DEVELOPER]

1. Run "build-platform.sh" to install the platform (sandbox). For OSX users this also generates the launchd services in /tmp, on a reboot run "build-platform.sh" again to regenerate the launchd services (it won't reinstall mongo or ghc).
2. Run "make" to build the project. (You can also run "make sandbox-init" first if you use the yet unrelease Cabal 1.17).
3. Optionally you might want to run ". environment.sh" to add OSMImport to your path (only relevant if you're using cabal sandbox).

[USER]

2. If you already have mongodb installed and just want to build and run OSMImport, then run "cabal install" and off you go.


Usage
-----

Four options are needed to start the import process

1. dbtype - database type to import into (either mongo or redis)
1. dbconnection - the host and port the database is running on (authentication is currently not supported).
2. dbname - the name of the database you want to import into.
3. filename - the name of the file to import.


[EXAMPLE]

OSMImport mongo '127.0.0.1:7720' 'geo_data' './download/england-latest.osm.pbf'
OSMImport redis '127.0.0.1:7721' '2' './download/england-latest.osm.pbf'

Notes
-----

