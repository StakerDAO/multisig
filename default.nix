# SPDX-FileCopyrightText: 2020 Tocqueville Group
#
# SPDX-License-Identifier: LicenseRef-MIT-TQ

rec {
  sources = import ./nix/sources.nix;
  # pkgs for builtins.currentSystem
  pkgs = import ./nix/nixpkgs-with-haskell-nix.nix {};
  pkgsStatic = pkgs.pkgsCross.musl64;

  # all local packages and their subdirectories
  # we need to know subdirectories for weeder and for cabal check
  local-packages = [
    { name = "multisig-proxy"; subdirectory = "./."; }
  ];

  # names of all local packages
  local-packages-names = map (p: p.name) local-packages;

  # source with gitignored files filtered out
  projectSrc = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "multisig-proxy";
    src = ./.;
  };

  # GHC has issues with compiling statically linked executables when Template Haskell
  # is used, this wrapper for ld fixes it. Another solution we could use is to link
  # GHC itself statically, but it seems to break haddock.
  # See https://github.com/input-output-hk/haskell.nix/issues/914#issuecomment-897424135
  linker-workaround = pkgs.writeShellScript "linker-workaround" ''
    # put all flags into 'params' array
    source ${pkgsStatic.stdenv.cc}/nix-support/utils.bash
    expandResponseParams "$@"

    # check if '-shared' flag is present
    hasShared=0
    for param in "''${params[@]}"; do
      if [[ "$param" == "-shared" ]]; then
        hasShared=1
      fi
    done

    if [[ "$hasShared" -eq 0 ]]; then
      # if '-shared' is not set, don't modify the params
      newParams=( "''${params[@]}" )
    else
      # if '-shared' is present, remove '-static' flag
      newParams=()
      for param in "''${params[@]}"; do
        if [[ ("$param" != "-static") ]]; then
          newParams+=( "$param" )
        fi
      done
    fi

    # invoke the actual linker with the new params
    exec x86_64-unknown-linux-musl-cc @<(printf "%q\n" "''${newParams[@]}")
  '';

  # haskell.nix package set
  # parameters:
  # - release -- 'true' for master and producion branches builds, 'false' for all other builds.
  #   This flag basically disables weeder related files production, haddock and enables stripping
  # - static -- build statically linked executables
  # - optimize -- 'true' to enable '-O1' ghc flag, we use it for publishing docker image for morley
  hs-pkgs = { release, static ? true, optimize ? false}:
    let
      haskell-nix = if static then pkgsStatic.haskell-nix else pkgs.haskell-nix;
    in haskell-nix.stackProject {
      src = projectSrc;

      ignorePackageYaml = false;

      modules = [
        # common options for all local packages:
        {
          packages = pkgs.lib.genAttrs local-packages-names (packageName: {
            ghcOptions = with pkgs.lib;
              # we use O1 for production binaries in order to improve their performance
              # for end-users
              [ (if optimize then "-O1" else "-O0") "-Werror"]
              # override linker to work around issues with Template Haskell
              ++ optionals static [ "-pgml=${linker-workaround}" ]
              # produce *.dump-hi files, required for weeder:
              ++ optionals (!release) ["-ddump-to-file" "-ddump-hi"]
              # do not erase any 'assert' calls
              ++ optionals (!release) ["-fno-ignore-asserts"];
            dontStrip = !release;  # strip in release mode, reduces closure size
            doHaddock = !release;  # don't haddock in release mode
          });
        }

        {
          # don't haddock dependencies
          doHaddock = false;
        }
      ];
  };

  hs-pkgs-development = hs-pkgs { release = false; };

  build-release = (hs-pkgs { release = true; optimize = true; }).multisig-proxy.components.exes.multisig-client;

  # component set for all local packages like this:
  # { morley = { library = ...; exes = {...}; tests = {...}; ... };
  #   morley-prelude = { ... };
  #   ...
  # }
  packages = pkgs.lib.genAttrs local-packages-names (packageName: hs-pkgs-development."${packageName}".components);

  # returns a list of all components (library + exes + tests + benchmarks) for a package
  get-package-components = pkg: with pkgs.lib;
    optional (pkg ? library) pkg.library
    ++ attrValues pkg.exes
    ++ attrValues pkg.tests
    ++ attrValues pkg.benchmarks;

  # per-package list of components
  components = pkgs.lib.mapAttrs (pkgName: pkg: get-package-components pkg) packages;

  # a list of all components from all packages in the project
  all-components = with pkgs.lib; flatten (attrValues components);

  haddock = with pkgs.lib; flatten (attrValues
    (mapAttrs (pkgName: pkg: optional (pkg ? library) pkg.library.haddock) packages));

  # stack2cabal is broken because of strict constraints, set 'jailbreak' to ignore them
  stack2cabal = pkgs.haskell.lib.overrideCabal pkgs.haskellPackages.stack2cabal (drv: {
    jailbreak = true;
    broken = false;
  });
}
