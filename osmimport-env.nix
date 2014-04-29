let
  pkgs = import <nixpkgs> {};
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
in 
  pkgs.myEnvFun {
    name = "osmimport-env";
    buildInputs = with pkgs; [
      haskellEnv
    ];
  }
