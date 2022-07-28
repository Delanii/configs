{
  description = "My first flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
  };

  outputs = { self, nixpkgs, home-manager, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
      lib = nixpkgs.lib;

    in {
      nixosConfigurations = {
        tomaskrulis = lib.nixosSystem {
          inherit system;
          modules = [
            ./configurations/configuration.nix
          ];
        };
      };
    };
}
