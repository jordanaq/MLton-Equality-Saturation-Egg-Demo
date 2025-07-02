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
            (rustVersion.override { extensions = [ "rust-src" ]; })  # required for rust-analyzer
            rust-analyzer                                            # IDE support
            cargo                                                    # core Rust tool
            rustPackages.clippy                                      # lints
            rustPackages.rustfmt                                     # formatting
            pkg-config                                               # linking C-based crates
            openssl                                                  # common native dependency
            gcc                                                      # builds C/C++ dependencies
            lldb                                                     # debugging
            cargo-watch                                              # optional: reload-on-save
            cargo-nextest                                            # optional: fast testing
          ];
          # Optionally configure shellHook for CARGO_HOME, PATH, etc.
        };

      });
}

