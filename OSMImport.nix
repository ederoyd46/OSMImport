let
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
  fetchurl = pkgs.fetchurl;
  fetchgit = pkgs.fetchgit;
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
  version = "1.0.0.1";
  gitSrc = fetchgit {
    url = "https://github.com/ederoyd46/OSMImport";
    rev = "321fad8b87685609ce12709572308608de75e093";
    sha256 = "06rk1dmqyw4z6wmpw127m7sb241pmwvyg8vd3znqriga2fzgid7g";
  };
in 

stdenv.mkDerivation rec {
  name = "OSMImport-${version}";
  src = gitSrc;

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
 
