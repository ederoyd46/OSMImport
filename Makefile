BASE_DIR=$(shell pwd)

CABAL_SANDBOX=$(BASE_DIR)/platform/osmimport
GHC=$(BASE_DIR)/platform/ghc/bin/ghc

HGEOCODER_BIN=$(BASE_DIR)/platform/osmimport/bin/OSMImport

default: install

#Default
install: 
	cabal install --with-compiler=$(GHC) 

#Initialise target directories
sandbox-init:
	cabal sandbox init --sandbox $(CABAL_SANDBOX)

#Server
docs:
	cabal haddock --executables --hyperlink-sources

build: tags
	cabal sandbox-build 

tags:
	hasktags -c src/

cleanMacFiles:
	find . -name '._*' -exec rm {} \;

clean:
	@rm -rf dist/*
	@rm -rf dist
