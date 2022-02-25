{
  description = "See README at <https://github.com/haroldcarr/network-zmq-unagi>";
  inputs      = {
    nixpkgs.url     = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs             = nixpkgs.legacyPackages.${system};
        haskellPackages  = pkgs.haskell.packages.ghc8107;
        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName      = "network-zmq-unagi";
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self rec {
            # Dependency overrides go here
          };
        defaultPackage          = self.packages.${system}.${packageName};
        devShell                = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.hpack
          ];
          inputsFrom            = builtins.attrValues self.packages.${system};
        };
      });
}
