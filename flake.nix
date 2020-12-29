{
  inputs = {
    nixpkgs.url = "github:serokell/nixpkgs";
    haskell-nix = {
      url =
        "github:input-output-hk/haskell.nix/bd45da822d2dccdbb3f65d0b52dd2a91fd65ca4e";
    };
    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    stackage = {
      url = "github:input-output-hk/stackage.nix";
      flake = false;
    };
  };
  outputs = { self, nixpkgs, haskell-nix, hackage, stackage }:
    let
      packagesFor = system: rec {
	pkgs = nixpkgs.legacyPackages.${system}.extend
	  (nixpkgs.lib.foldl nixpkgs.lib.composeExtensions (_: _: { }) [
	    (haskell-nix.overlays {
	      sourcesOverride = haskell-nix.sources // {
		inherit hackage stackage;
	      };
	    }).combined-eval-on-build
	  ]);
	project = pkgs.callPackage {} ./project;
	library = project.library;
	multisig-client = project.multisig-client;
	test = project.test;
      };
    in rec {
      packages = builtins.mapAttrs (system: _: 
        with packagesFor system; { inherit multisig-client; }
      ) nixpkgs.legacyPackages;

      checks = builtins.mapAttrs (system: _: 
        with packagesFor system; { inherit test; }
      ) nixpkgs.legacyPackages;
    };
}
