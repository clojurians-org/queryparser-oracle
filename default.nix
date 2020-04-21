{ pkgs ? import <nixpkgs> {}} : 
with pkgs ;
let
  queryparser_git = fetchFromGitHub {
      owner = "uber" ;
      repo = "queryparser" ;
      rev = "6015e8f273f4498326fec0315ac5580d7036f8a4" ;
      sha256 = "05pnifm5awyqxi6330v791b1cvw26xbcn2r20pqakvl8d3xyaxa4" ;
  } ; 
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: with haskell.lib; {
      predicate-class = doJailbreak super.predicate-class ;
      queryparser = appendConfigureFlag 
                      (dontHaddock (doJailbreak (self.callCabal2nix "queryparser" queryparser_git {})))
                      "--ghc-options=-XNoMonadFailDesugaring" ;
    } ;
  };
in

haskellPackages.developPackage {
  root = ./.;

  modifier = drv: haskell.lib.overrideCabal drv (attrs: {
    buildTools = (attrs.buildTools or []) ++ [haskellPackages.cabal-install] ;
  }) ;
}
