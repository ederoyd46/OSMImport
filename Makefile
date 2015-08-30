BASE_DIR=$(shell pwd)
CABAL_SANDBOX=$(BASE_DIR)/platform/osmimport
GHC_FLAGS=-O2 -rtsopts

# GHC Build #################################################################################

default: build

clean:
	-@rm -r bin BUILD

init:
	-@mkdir -p BUILD bin
	-@rm -rf BUILD/*
	-@cp -r src/* BUILD

build: init
	@cd BUILD && ghc --make Main $(GHC_FLAGS) && mv Main ../bin/OSMImport


# Docker #####################################################################################
start_mongo:
	docker run -d -v $(BASE_DIR)/data:/data/db -p 27017:27017 mongo

run_in_docker:
	docker run -it -rm -v `pwd`:/project -w /project/src haskell:geo-index ghci Importer.hs

build_in_docker:
	docker build --rm=true .

# Cabal ######################################################################################

# Default
cabal-build:
	cabal configure
	cabal build

cabal-install:
	cabal install

#Can probably ignore this one now
#cabal-prerequisites-init:
#	cabal install hello happy alex hprotoc hlint hoogle ghc-mod HsColour hasktags hdevtools stylish-haskell haskell-docs

cabal-sandbox-init:
	cabal sandbox init --sandbox $(CABAL_SANDBOX)
	cabal install --only-dependencies --force-reinstalls

cabal-docs:
	cabal haddock --executables --hyperlink-source

cabal-ghci:
	cabal repl

##############################################################################################

test-data:
	mkdir -p $(BASE_DIR)/download
	curl -C - http://download.geofabrik.de/europe/great-britain/england-latest.osm.pbf > $(BASE_DIR)/download/england-latest.osm.pbf

test-mongo:
	$(BASE_DIR)/dist/build/OSMImport/OSMImport '127.0.0.1:27017' 'geo_data' '$(BASE_DIR)/download/england-latest.osm.pbf'
	#+RTS -N2 -RTS -- Added to end to make use of multicores

kill:
	killall OSMImport

clean-platform: clean
	-@rm cabal.sandbox.config
	-@rm -r platform
	-@find . -name '._*' -exec rm {} \;
	-@rm -r $(BASE_DIR)/dist

generate-protocol-buffers:
	cd $(BASE_DIR)/etc && hprotoc --unknown_fields --include_imports --haskell_out=$(BASE_DIR)/src $(BASE_DIR)/etc/osmformat.proto $(BASE_DIR)/etc/fileformat.proto

clean-generated-protocol-buffers:
	@rm -r $(BASE_DIR)/src/OSM
