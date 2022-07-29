{
  description = "My first flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
    home-manager = {
      url = "github:nix-community/home-manager/release-22.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    unstable.url = "github:nixos/nixpkgs";
  };

  outputs = { self, nixpkgs, unstable, home-manager, ... }:
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
          specialArgs = {
            inherit unstable;
          };
          modules = [
            ./configurations/configuration.nix
            ./configurations/unstable.nix
            home-manager.nixosModules.home-manager {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.tomaskrulis = {
                imports = [
                  ./home-manager/home.nix
                ];
              };
            }
          ];
        };
      };
      hmConfig = {
        tomaskrulis = home-manager.lib.homeMnagerConfiguration {
          inherit system pkgs;
          username = "tomaskrulis";
          homeDirectory = "/home/tomaskrulis";
          stateVersion = "22.05";
          configuration = {
            imports = [
              ./home-manager/home.nix
            ];
          };
        };
      };
    };
}
