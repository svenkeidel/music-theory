{ pkgs ? import <nixpkgs> {} }:

let
  hsEnv = pkgs.haskellPackages.ghcWithPackages(p: with p; [
    Cabal cabal-install hlint text containers hspec mtl
  ]);

in pkgs.stdenv.mkDerivation {
  name = "music";
  version = "0.0.1";
  src = ./.;
  buildInputs = [
    hsEnv pkgs.lilypond
  ];
}
