{
  description = "Art for my memo on continuous consistency";

  nixConfig.extra-substituters = [
    "https://tweag-jupyter.cachix.org"
  ];
  nixConfig.extra-trusted-public-keys = [
    "tweag-jupyter.cachix.org-1:UtNH4Zs6hVUFpFBTLaA4ejYavPo5EFFqgd7G7FxGW9g="
  ];

  inputs.flake-compat.url = github:edolstra/flake-compat;
  inputs.flake-compat.flake = false;
  inputs.flake-utils.url = github:numtide/flake-utils;
  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-22.11;
  inputs.jupyenv.url = github:tweag/jupyenv;

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
              art-gen =
                pkgs.haskellPackages.callPackage (import ./art-gen.nix) {};
              art = pkgs.stdenv.mkDerivation rec {
                name = "art";
                src = ./.;
                phases = "buildPhase installPhase";
                version = "0.1";
                buildInputs = [ pkgs.ruby
                                pkgs.which
                                art-gen
                              ];
                generator = art-gen;
                buildPhase = ''
                  ruby $src/make.rb
                  '';
                installPhase = ''
                  mkdir -p $out
                  cp -r _out/* $out
                  '';
                meta = {
                  description = "Art for a memo";
                  longDescription = ''
                  This package contains diagrams for a memo
                  generated with Haskell's diagram package.
                  '';
                  homepage = "http://tealeaves.science";
                  license = pkgs.lib.licenses.mit;
                };
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
