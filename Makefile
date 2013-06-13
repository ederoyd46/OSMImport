BASE_DIR=$(shell pwd)

CABAL_SANDBOX=$(BASE_DIR)/platform/osmimport
GHC=$(BASE_DIR)/platform/ghc/bin/ghc

HGEOCODER_BIN=$(BASE_DIR)/platform/osmimport/bin/OSMImport

default: install

#Default
install: 
	cabal install

#Initialise target directories
sandbox-init:
	cabal sandbox init --sandbox $(CABAL_SANDBOX)

#Server
docs:
	cabal haddock --executables --hyperlink-sources

tags:
	hasktags -c src/

cleanMacFiles:
	find . -name '._*' -exec rm {} \;

clean:
	@rm -rf dist/*
	@rm -rf dist

ghci:
	ghci -no-user-package-db -package-db $(CABAL_SANDBOX)/x86_64-osx-ghc-7.6.3-packages.conf.d