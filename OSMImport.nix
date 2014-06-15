let
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
  fetchurl = pkgs.fetchurl;
  haskellEnv = pkgs.haskellPackages_ghc763_no_profiling.ghcWithPackages (self : (
    [
      self.protocolBuffers
      self.protocolBuffersDescriptor
      self.utf8String
      self.binary
      self.aeson
      self.hasktags
      self.text
      self.cereal
      self.zlib
      self.mtl
      self.mongoDB
      self.split
    ]
  ));
  version = "v1.0.0.1";
  mainSrc = fetchurl {
    url = "https://github.com/ederoyd46/OSMImport/archive/${version}.tar.gz";
    sha256 = null;
  };
in 

stdenv.mkDerivation rec {
  name = "OSMImport-${version}";
  src = mainSrc;

  buildPhase = ''
    export PATH=${haskellEnv.outPath}/bin:$PATH
    make
  '';

  # Not needed - here for future reference
  buildDepends = [
    haskellEnv
  ];

  installPhase = ''
    ensureDir $out/bin
    cp -r ./bin $out
  '';

  meta = {
    description = "This application parses data from the Open Street Map Protocol Buffer Format (http://wiki.openstreetmap.org/wiki/PBF_Format) and imports it into a MongoDB database.";
    homepage    = https://github.com/ederoyd46/OSMImport;
    license     = stdenv.lib.licenses.mit;
    platforms   = stdenv.lib.platforms.all;
    maintainers = with stdenv.lib.maintainers; [ ederoyd46 ];
  };
}
 
