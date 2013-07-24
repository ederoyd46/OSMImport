BASE_DIR=$(shell pwd)

CABAL_SANDBOX=$(BASE_DIR)/platform/osmimport
GHC=$(BASE_DIR)/platform/ghc/bin/ghc

HGEOCODER_BIN=$(BASE_DIR)/platform/osmimport/bin/OSMImport

default: install

#Default
install: tags 
	cabal install

#Initialise target directories
sandbox-init:
	cabal sandbox init --sandbox $(CABAL_SANDBOX)

#Server
docs:
	cabal haddock --executables --hyperlink-sources

tags:
	@hasktags -c src/

cleanMacFiles:
	find . -name '._*' -exec rm {} \;

clean:
	@rm $(BASE_DIR)/tags
	@rm -r $(BASE_DIR)/dist/*
	@rm -r $(BASE_DIR)/dist
	
#Only use for sandboxed ghci...currently set up for OSX (need a more generic way of doing this)
ghci:
	cd src && ghci -no-user-package-db -package-db $(CABAL_SANDBOX)/x86_64-osx-ghc-7.6.3-packages.conf.d Main

generate-protocol-buffers:
	cd $(BASE_DIR)/etc && hprotoc --unknown_fields --include_imports --haskell_out=$(BASE_DIR)/src $(BASE_DIR)/etc/osmformat.proto $(BASE_DIR)/etc/fileformat.proto

clean-generated-protocol-buffers:
	@rm -r $(BASE_DIR)/src/OSM

