BASE_DIR=$(shell pwd)

# Default
default: build

# Download data
download-uk-data:
	mkdir -p $(BASE_DIR)/download
	curl -L -C - http://download.geofabrik.de/europe/great-britain/england-latest.osm.pbf > $(BASE_DIR)/download/england-latest.osm.pbf

download-de-data:
	mkdir -p $(BASE_DIR)/download
	curl -L -C - http://download.geofabrik.de/europe/germany-latest.osm.pbf > $(BASE_DIR)/download/germany-latest.osm.pbf

# Protocol Buffers
generate-protocol-buffers:
	cd $(BASE_DIR)/etc && hprotoc --unknown_fields --include_imports --haskell_out=$(BASE_DIR)/src $(BASE_DIR)/etc/osmformat.proto $(BASE_DIR)/etc/fileformat.proto

clean-generated-protocol-buffers:
	@rm -r $(BASE_DIR)/src/OSM

# import-uk-data:
# 	OSMImport '127.0.0.1:27017' 'geo_data_uk' '$(BASE_DIR)/download/england-latest.osm.pbf' 
# import-de-data:
# 	OSMImport '127.0.0.1:27017' 'geo_data_de' '$(BASE_DIR)/download/germany-latest.osm.pbf'