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
	package.ghcOptions = "-Werror";
      };
    }];
  };
  library = project.multisig-proxy.components.library;
  multisig-client = project.multisig-proxy.components.exes.multisig-client;
  test = project.staker-bridge-web.checks.test;
  in { inherit library multisig-client test; }
