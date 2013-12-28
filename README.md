Description
-----------

This application parses data from the Open Street Map Protocol Buffer Format (http://wiki.openstreetmap.org/wiki/PBF_Format) and imports it into a MongoDB database.

It has been written in Haskell and was inspired by the https://github.com/larroy/osmcompiler project.

Current Status
--------------

0.1.0.0 - Initial version, imports (some) node data only. Lots of work to do :)

0.2.0.0 - Added missing node data, now commits to MongoDB in larger batches.

0.3.0.0 - Added support to import into redis, lots of performance improvements.

0.4.0.0 - Changed to use multiple threads. Moved a bulk of the logic into it's own module

0.5.0.0 - Reimplemented using alternative procol buffer library due to slow parsing of some records and problems with the sint datatype.

0.6.0.0 - Added Ways and Relation initial implementation for MongoDB Only. Improved build process.

0.7.0.0 - Removed redis support. Removed use of multiple threads.

Installation Instructions
-------------------------

Yes this has been written on a MAC! Although most scripts will just work with Linux, the services aren't yet generated for Linux and I've not added any prerequisits for Linux.

1. Run build-platform.sh to install the required cabal libraries and build the project
2. Optionally you might want to run ". environment.sh" to add OSMImport to your path (only relevant if you're using cabal sandbox).

Usage
-----

Three options are needed to start the import process

1. dbconnection - the host and port the database is running on (authentication is currently not supported).
2. dbname - the name of the database you want to import into.
3. filename - the name of the file to import.


[EXAMPLE]

OSMImport '127.0.0.1:7720' 'geo_data' './download/england-latest.osm.pbf'
