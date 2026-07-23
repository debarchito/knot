{
  inputs = {
    nixpkgs.url = "https://channels.nixos.org/nixos-unstable/nixexprs.tar.xz";
    nixpkgs-lib.follows = "nixpkgs";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs-lib";
    };
    opam-nix = {
      url = "github:debarchito/opam-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      flake-parts,
      opam-nix,
      opam-repository,
      treefmt-nix,
      ...
    }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        flake-parts.flakeModules.easyOverlay
        treefmt-nix.flakeModule
      ];

      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];

      perSystem =
        {
          lib,
          pkgs,
          system,
          ...
        }:
        let
          on = opam-nix.lib.${system};

          basePackagesQuery = {
            ocaml-variants = "5.5.0+options,ocaml-option-flambda";
            ocaml-config = "*";
            knot = "*";
          };

          devPackagesQuery = {
            ocamlformat = "*";
            ocaml-lsp-server = "*";
          };

          scope = on.buildOpamProject' { repos = [ opam-repository ]; } (lib.cleanSource ./.) (
            basePackagesQuery // devPackagesQuery
          );

          devPackages = builtins.attrValues (pkgs.lib.getAttrs (builtins.attrNames devPackagesQuery) scope);
        in
        {
          packages = rec {
            inherit (scope) knot;
            default = knot;
          };

          treefmt = {
            projectRootFile = "flake.nix";
            programs = {
              nixfmt.enable = true;
              ocamlformat = {
                enable = true;
                package = scope.ocamlformat // {
                  meta.mainProgram = "ocamlformat";
                };
              };
            };
          };

          overlayAttrs = {
            inherit (scope) knot;
          };

          devShells.default = pkgs.mkShell {
            name = "knot-dev";

            inputsFrom = [ scope.knot ];
            nativeBuildInputs = devPackages;
          };
        };
    };
}
