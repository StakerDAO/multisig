# SPDX-FileCopyrightText: 2020 Tocqueville Group
#
# SPDX-License-Identifier: LicenseRef-MIT-TQ

{ system ? builtins.currentSystem }:
let
  sources = import ./sources.nix;
  haskellNix = import sources."haskell.nix" {
    sourcesOverride = { hackage = sources."hackage.nix"; stackage = sources."stackage.nix"; };
  };
  nixpkgs = import sources.nixpkgs;

in
  nixpkgs (haskellNix.nixpkgsArgs // {
    localSystem.system = system;
  })
