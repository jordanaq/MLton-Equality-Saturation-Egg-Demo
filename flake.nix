{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, rust-overlay, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs { inherit system overlays; };
        rustVersion = pkgs.rust-bin.stable.latest.default;
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            (rustVersion.override { extensions = [ "rust-src" ]; })
            rust-analyzer
            cargo
            rustPackages.clippy
            rustPackages.rustfmt
            pkg-config
            openssl
            gcc
            lldb
            cargo-watch
            cargo-nextest
          ];
        };
      });
}

