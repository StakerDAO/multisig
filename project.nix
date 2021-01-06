{ haskell-nix }: 
let
  project = haskell-nix.stackProject {
    src = haskell-nix.haskellLib.cleanGit {
      src = haskell-nix.cleanSourceHaskell { src = ./.; };
    };
    modules = [{
      doHaddock = false;
      packages.multisig-proxy = {
        doHaddock = true;
        package.ghcOptions = "-Werror -O0";
      };
    }];
  };
  library = project.multisig-proxy.components.library;
  multisig-client = project.multisig-proxy.components.exes.multisig-client;
  test = project.multisig-proxy.checks.test;
  shell = project.shellFor {
    packages = ps: with ps; [ multisig-proxy ];
    withHoogle = false;
    tools = { cabal = "3.2.0.0"; hpack = "0.34.3"; };
    exactDeps = true;
  };
  in { inherit library multisig-client test shell; }
