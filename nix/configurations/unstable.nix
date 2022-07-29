{ config, pkgs, unstable, ... }:

let
  unstable-pkgs = import unstable {
    inherit (pkgs) system;
    config.allowUnfree = true;
  };
in
{
  environment.systemPackages = with pkgs; [
    unstable-pkgs.vale
  ];
}
