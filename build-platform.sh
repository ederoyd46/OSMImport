#!/bin/bash
. environment.sh

function init() {
    echo Create Platform Directories
    if [ ! -d "$download" ]; then
        mkdir -p $download
    fi

    if [ ! -d "$platform" ]; then
        mkdir -p $platform
    fi

    if [ ! -d "$platform_src" ]; then
        mkdir -p $platform_src
    fi

    if [ ! -d "$log_dir" ]; then
        mkdir -p $log_dir
    fi
}

function installGHC() {
    ghc_src=$platform_src/ghc
    if [ ! -d "$ghc" ]; then
        echo Install GHC
        rm -rf $ghc_src
        if [ "$uname" == 'Darwin' ]; then
            if [ ! -f "$download/ghc-$ghc_version-x86_64-apple-darwin.tar.bz2" ]; then
                cd $download
                curl -C - -O http://www.haskell.org/ghc/dist/$ghc_version/ghc-$ghc_version-x86_64-apple-darwin.tar.bz2
            fi
            mkdir -p $ghc_src; cd $ghc_src
            tar jxf $download/ghc-$ghc_version-x86_64-apple-darwin.tar.bz2
        else
            if [ ! -f "$download/ghc-$ghc_version-x86_64-unknown-linux.tar.bz2" ]; then
                cd $download
                curl -C - -O http://www.haskell.org/ghc/dist/$ghc_version/ghc-$ghc_version-x86_64-unknown-linux.tar.bz2
            fi
            mkdir -p $ghc_src; cd $ghc_src
            tar jxf $download/ghc-$ghc_version-x86_64-unknown-linux.tar.bz2
        fi

        cd ghc-$ghc_version
        ./configure --prefix=$ghc
        make install
        echo Done
    fi
}

function installMongo() {
    mongo_src=$platform_src/mongo
    if [ ! -d "$mongo_install" ]; then    
        echo Install MongoDB
            
        rm -rf $mongo_src
        rm -rf $mongo_data

        mkdir -p $mongo_data
        #Install Mongo
        if [ "$uname" == 'Darwin' ]; then
            if [ ! -f "$download/mongodb-osx-x86_64-$mongo_version.tgz" ]; then
                cd $download
                curl -C - -O http://fastdl.mongodb.org/osx/mongodb-osx-x86_64-$mongo_version.tgz
            fi
            mkdir -p $mongo_src; cd $mongo_src
            tar zxf $download/mongodb-osx-x86_64-$mongo_version.tgz
            mv mongodb-osx-x86_64-$mongo_version $mongo_install
        else
            if [ ! -f "$download/mongodb-linux-x86_64-$mongo_version.tgz" ]; then
                cd $download
                curl -C - -O http://fastdl.mongodb.org/linux/mongodb-linux-x86_64-$mongo_version.tgz
            fi
            mkdir -p $mongo_src; cd $mongo_src
            tar zxf $download/mongodb-linux-x86_64-$mongo_version.tgz
            mv mongodb-linux-x86_64-$mongo_version $mongo_install
        fi
        echo Done
    fi
}

function generateServices() {
    if [ "$uname" == 'Darwin' ]; then
        mkdir -p /tmp/launchd
        sed -e 's!PROCESS_OWNER!'$process_owner'!g' -e 's!PLATFORM_DIR!'$platform'!g' -e 's!MONGO_PORT!'$mongo_port'!g' -e 's!LOG_DIR!'$log_dir'!g' -e 's!MONGO_DATA!'$mongo_data'!g' -e 's!MONGO_BIN!'$mongo_bin'!g' $config/launchd/osmimport-db.plist > /tmp/launchd/osmimport-db.plist
        echo Generated Services - Use to start and stop e.g. 'launchctl load /tmp/launchd/osmimport-db.plist'
        echo Unloading Services...
        launchctl unload /tmp/launchd/osmimport-db.plist
        echo Loading Services...
        launchctl load /tmp/launchd/osmimport-db.plist
    fi
}

init 
installGHC
installMongo
generateServices
