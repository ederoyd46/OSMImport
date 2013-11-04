BASE_DIR=$(shell pwd)

CABAL_SANDBOX=$(BASE_DIR)/platform/osmimport

default: build

#Default
build: tags 
	cabal configure
	cabal build

install: tags
	cabal install

prerequisits-init:
	cabal install hello happy alex hprotoc hlint hoogle ghc-mod HsColour hasktags hdevtools stylish-haskell haskell-docs

sandbox-init:
	cabal sandbox init --sandbox $(CABAL_SANDBOX)
	cabal install --only-dependencies --force-reinstalls

docs:
	cabal haddock --executables --hyperlink-source

tags:
	@hasktags -c src/

cleanMacFiles:
	find . -name '._*' -exec rm {} \;

clean:
	@rm $(BASE_DIR)/tags
	@rm -r $(BASE_DIR)/dist
	
test-data:
	mkdir -p $(BASE_DIR)/download
	curl -C - http://download.geofabrik.de/europe/great-britain/england-latest.osm.pbf > $(BASE_DIR)/download/england-latest.osm.pbf

test-mongo: build
	$(BASE_DIR)/dist/build/OSMImport/OSMImport mongo '127.0.0.1:8820' 'geo_data' '$(BASE_DIR)/download/england-latest.osm.pbf'

kill:
	killall OSMImport

ghci:
	cabal repl

generate-protocol-buffers:
	cd $(BASE_DIR)/etc && hprotoc --unknown_fields --include_imports --haskell_out=$(BASE_DIR)/src $(BASE_DIR)/etc/osmformat.proto $(BASE_DIR)/etc/fileformat.proto

clean-generated-protocol-buffers:
	@rm -r $(BASE_DIR)/src/OSM

