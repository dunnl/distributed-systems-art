{pkgs, ...}: {
  kernel.python.minimal = {
    enable = true;
  };
  kernel.haskell.minimal = {
    enable = true;
    extraHaskellPackages = ps: with ps; [ diagrams diagrams-pgf ihaskell-diagrams ];
  };
}
