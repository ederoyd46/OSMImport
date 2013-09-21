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


init 
