{
  description = "Development shell and package for ai-rake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowBroken = true;  # needed because openai is marked broken
        };
        haskellPackages = pkgs.haskell.packages.ghc9103.override {
          overrides = self: super: {
            openai = pkgs.haskell.lib.dontCheck super.openai;
          };
        };
        ai-rake = haskellPackages.callCabal2nix "ai-rake" ./. { };
        libraryPath = pkgs.lib.makeLibraryPath [
          pkgs.libpq
          pkgs.zlib
        ];
      in
      {
        packages.default = ai-rake;

        devShells.default = haskellPackages.shellFor {
          packages = _: [ ai-rake ];
          withHoogle = false;

          nativeBuildInputs = [
            pkgs.pkg-config
          ];

          buildInputs = [
            haskellPackages.cabal-install
            haskellPackages.ghcid
            haskellPackages.haskell-language-server
            haskellPackages.hspec-discover
            haskellPackages.implicit-hie
            pkgs.fish
            pkgs.fourmolu
            pkgs.hlint
            pkgs.libpq
            pkgs.postgresql
            pkgs.process-compose
            pkgs.zlib
          ];

          shellHook = ''
            export PC_PORT_NUM=6599
          '';
        };
      });
}
