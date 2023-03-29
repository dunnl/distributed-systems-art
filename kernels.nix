{pkgs, ...}: {
  kernel.python.minimal = {
    enable = true;
  };
  kernel.haskell.minimal = {
    enable = true;
    extraHaskellPackages = ps: with ps; [ diagrams ihaskell-diagrams ];
  };
}
