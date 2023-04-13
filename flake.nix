{
  description = "Debug Adapter for Haskell debugging system.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    haskell-dap.url = "github:phoityne/haskell-dap";

    ghci-dap.url = "github:phoityne/ghci-dap";

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
    haskell-dap,
    ghci-dap,
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
          haskell-dap.overlays.default
          ghci-dap.overlays.default
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
        packages = p:
          with p; [
            haskell-debug-adapter
          ];
        withHoogle = true;
        buildInputs =
          (with pkgs; [
            haskell-language-server
            cabal-install
            haskellPackages.implicit-hie
            zlib
          ])
          ++ (with pre-commit-hooks.packages.${system}; [
            hpack
            cabal2nix
            alejandra
          ]);
        shellHook = ''
          gen-hie --cabal > hie.yaml
          ${self.checks.${system}.pre-commit-check.shellHook}
        '';
      };
    in {
      devShells = {
        default = devShell;
      };

      packages = rec {
        default = haskell-debug-adapter;
        haskell-debug-adapter = pkgs.haskellPackages.haskell-debug-adapter;
      };

      checks = {
        inherit pre-commit-check;
        haskell-debug-adapter = self.packages.${system}.default;
      };
    })
    // {
      overlays.default = overlay;
    };
}
