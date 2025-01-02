{ pkgs ? import <nixpkgs> { } }:

pkgs.rustPlatform.buildRustPackage {
  pname = "not-bad-launcher";
  version = "0.1";

  cargoLock.lockFile = ./Cargo.lock;

  src = pkgs.lib.cleanSource ./.;
}
