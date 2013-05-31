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

function installRedis() {
    redis_src=$platform_src/redis
    if [ ! -d "$redis" ]; then

        rm -rf $redis_src
        rm -rf $redis_data
        mkdir -p $redis_data
        
        if [ ! -f "$download/redis-$redis_version.tar.gz" ]; then
          cd $download
          curl -C - -O http://redis.googlecode.com/files/redis-$redis_version.tar.gz
        fi

        mkdir -p $redis_src; cd $redis_src
        tar zxf $download/redis-$redis_version.tar.gz
        cd redis-$redis_version
        make -j9
        mkdir $redis
        cp ./src/redis-server $redis
        cp ./src/redis-cli $redis
    fi
}

function generateServices() {
    if [ "$uname" == 'Darwin' ]; then
        mkdir -p /tmp/launchd
        sed -e 's!PROCESS_OWNER!'$process_owner'!g' -e 's!PLATFORM_DIR!'$platform'!g' -e 's!MONGO_PORT!'$mongo_port'!g' -e 's!LOG_DIR!'$log_dir'!g' -e 's!MONGO_DATA!'$mongo_data'!g' -e 's!MONGO_BIN!'$mongo_bin'!g' $config/launchd/osmimport-mongodb.plist > /tmp/launchd/osmimport-mongodb.plist        
        sed -e 's!LOG_DIR!'$log_dir'!g' -e 's!REDIS_PORT!'$redis_port'!g' -e 's!REDIS_DATA!'$redis_data'!g' $config/redis/redis.conf > $redis/redis.conf
        sed -e 's!PROCESS_OWNER!'$process_owner'!g' -e 's!PLATFORM_DIR!'$platform'!g' -e 's!REDIS_BIN!'$redis_bin'!g' $config/launchd/osmimport-redis.plist > /tmp/launchd/osmimport-redis.plist
        echo Generated Services - Use to start and stop e.g. 'launchctl load /tmp/launchd/osmimport-mongodb.plist'
        echo Unloading Services...
        launchctl unload /tmp/launchd/osmimport-mongodb.plist
        launchctl unload /tmp/launchd/osmimport-redis.plist
        echo Loading Services...
        launchctl load /tmp/launchd/osmimport-mongodb.plist
        launchctl load /tmp/launchd/osmimport-redis.plist
    fi
}

init 
installMongo
installRedis
generateServices
