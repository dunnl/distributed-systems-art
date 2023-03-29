{
  description = "Art for my memo on continuous consistency";

  nixConfig.extra-substituters = [
    "https://tweag-jupyter.cachix.org"
  ];
  nixConfig.extra-trusted-public-keys = [
    "tweag-jupyter.cachix.org-1:UtNH4Zs6hVUFpFBTLaA4ejYavPo5EFFqgd7G7FxGW9g="
  ];

  inputs =
    { flake-compat.url = github:edolstra/flake-compat;
      flake-compat.flake = false;
      flake-utils.url = github:numtide/flake-utils;
      nixpkgs.url = github:NixOS/nixpkgs/nixos-22.11;
      jupyenv.url = github:tweag/jupyenv;
    };

  outputs = {
    self,
    flake-compat,
    flake-utils,
    nixpkgs,
    jupyenv,
    ...
  } @ inputs:
    flake-utils.lib.eachSystem
    [
      flake-utils.lib.system.x86_64-linux
    ]
    (
      system: let
        inherit (jupyenv.lib.${system}) mkJupyterlabNew;
        jupyterlab = mkJupyterlabNew ({...}: {
          nixpkgs = inputs.nixpkgs;
          imports = [(import ./kernels.nix)];
        });
        pkgs = import nixpkgs { inherit system; };
      in
        {
          packages = rec
            { default = jupyterlab;
              inherit jupyterlab;
              generator =
                pkgs.haskellPackages.callPackage (import ./generator.nix) {};
              art = pkgs.stdenv.mkDerivation {
                name = "art";
                src = ./.;
                phases = "buildPhase installPhase";
                version = "0.1";
                buildInputs = [ pkgs.ruby
                                pkgs.which
                                generator
                              ];
                inherit generator;
                buildPhase = ''
                  ruby $src/make.rb
                  '';
                installPhase = ''
                  mkdir -p $out
                  cp -r _out/* $out
                  '';
                };
              };
          apps =
            { default =
                { program = "${jupyterlab}/bin/jupyter-lab";
                  type = "app";
                };
            };
          devShell = pkgs.haskellPackages.shellFor {
            packages = p: [
            ];
            buildInputs = with pkgs.haskellPackages; [
              cabal-install
              pkgs.ruby
              pkgs.which
            ];
            withHoogle = true;
          };
        }
    );
}
