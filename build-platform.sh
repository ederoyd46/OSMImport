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

function installPrerequisites() {
    echo Install Prerequisites
    if [ "$uname" == 'Darwin' ]; then
      echo Install OSX Brew Prerequisites
      brew update
      brew upgrade
      brew install autoconf automake pcre gnupg
    fi

    echo Install Cabal Prerequisites
    cabal update
    make cabal-prerequisites-init

    echo Initialize the Sandbox
    make cabal-sandbox-init
}

function build() {
    echo Build the project
    make build
}

init 
installPrerequisites
build

echo Complete!!
