{
  description = "Debug Adapter for Haskell debugging system.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    flake-utils.url = "github:numtide/flake-utils";

    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    pre-commit-hooks,
    flake-utils,
    ...
  }: let
    supportedSystems = [
      "aarch64-linux"
      "aarch64-darwin"
      "x86_64-darwin"
      "x86_64-linux"
    ];
    overlay = import ./nix/overlay.nix;
  in
    flake-utils.lib.eachSystem supportedSystems (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          overlay
        ];
      };

      pre-commit-check = pre-commit-hooks.lib.${system}.run {
        src = self;
        hooks = {
          cabal2nix.enable = true;
          alejandra.enable = true;
          hpack.enable = true;
        };
        settings = {
          alejandra.exclude = ["default.nix"];
        };
      };

      devShell = pkgs.haskellPackages.shellFor {
        name = "haskell-debug-adapter-devShell";
        packages = p: with p; [haskell-debug-adapter];
        withHoogle = true;
        buildInputs =
          (with pkgs; [
            haskell-language-server
            cabal-install
            zlib
          ])
          ++ (with pre-commit-hooks.packages.${system}; [
            hpack
            cabal2nix
            alejandra
          ]);
        shellHook = ''
          ${self.checks.${system}.pre-commit-check.shellHook}
        '';
      };
    in {
      devShells = {
        default = devShell;
      };

      overlays = overlay;

      packages = rec {
        default = haskell-debug-adapter;
        haskell-debug-adapter = pkgs.haskellPackages.haskell-debug-adapter;
      };

      checks = {
        inherit pre-commit-check;
        haskell-debug-adapter = self.packages.${system}.default;
      };
    });
}
