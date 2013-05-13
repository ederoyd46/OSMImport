Description
-----------

This application parses data from the Open Street Map Protocol Buffer Format (http://wiki.openstreetmap.org/wiki/PBF_Format) and imports it into a MongoDB database.

It has been written in Haskell and was inspired by the https://github.com/larroy/osmcompiler project.

Current Status
--------------

0.1.0.0 - Initial version, imports (some) node data only. Lots of work to do :)


Installation Instructions
-------------------------

Yes this has been written on a MAC! Although most scripts will just work with Linux, the services aren't yet generated for Linux and I've not added any prerequisits for Linux.

[OSX Users]

1. If you have homebrew (http://brew.sh/) installed then run the prerequisits.sh to install required libraries.

[DEVELOPER]

1. Run "build-platform.sh" to install the platform (sandbox). For OSX users this also generates the launchd services in /tmp, on a reboot run "build-platform.sh" again to regenerate the launchd services (it won't reinstall mongo or ghc).
2. Run "make" to build the project (note this uses cabal's yet to be released sandbox feature, so make sure you have a installed cabal 1.17 from git://github.com/haskell/cabal.git).
3. Optionally you might want to run ". environment.sh" to add OSMImport to your path

[USER]

2. If you already have mongodb installed and just want to build and run OSMImport, then run "cabal install" and off you go.


Usage
-----

Three options are needed to start the import process

1. dbconnection - the host and port the database is running on (authentication is currently not supported)
2. dbname - the name of the database you want to import into 
3. filename - the name of the file to import


[EXAMPLE]

OSMImport '127.0.0.1:7720' 'geo_data' 'england-latest.osm.pbf'


Known Issues
------------

1. Currently the import it's a very slow process. One blob is imported at a time (usually 8000 nodes a blob) on a single thread. This issue will be addressed soon.
2. I'm a Haskell newbie, so I appreciate the code will be bad....I'm working on it :)


Notes
-----

I'm currently using a newer GHC than the Haskell-Platform project uses. Although I haven't used anything yet, moving forward I want take advantage of some of the new features.

