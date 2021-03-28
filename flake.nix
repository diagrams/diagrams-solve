{
  description = "A very basic flake";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils/master";
  }; 
  outputs = { self, nixpkgs, flake-utils, ... }:
    with flake-utils.lib;
    with nixpkgs.lib;
    eachSystem [ "x86_64-linux" ] (system:
      let version = "${substring 0 8 self.lastModifiedDate}.${self.shortRev or "dirty"}";
          overlay = self: super:
            with self;
            with haskell.lib;
            with haskellPackages;
            {
              diagrams-solve = rec {
                package = overrideCabal (callCabal2nix "diagrams-solve" ./. {}) (o: { version = "${o.version}-${version}"; });
              };
            };
          overlays = [ overlay ];
      in
        with (import nixpkgs { inherit system overlays; });
        rec {
          packages = flattenTree (recurseIntoAttrs { diagrams-solve = diagrams-solve.package; });
          defaultPackage = packages.diagrams-solve;
        });
}
