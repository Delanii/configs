{ config, pkgs, ... } : {
  #
  # Home-manager settings
  #
  home.packages = with pkgs; [
    alacritty
  ];

  # Test of writing specific configuration files with home-manager
  home.file = { # $HOME path is prepended before the file path. There is no need to write `~/`
    "nix-write-file-test/test.txt".text =
      ''
      This is a test of { special } symbols # and this too \ ...
      '';
  };

  # home.file.".doom.d" = { # this could be used for .doom.d for example; but probably works only when used directly in home-manager, not when using a flake
  #   source = "/home/tomaskrulis/Documents/MegaSynchonized/MegaSynchronized/configs/emacs/DoomEmacs/.doom.d";
  #   recursive = true;
  #  # executable = true; # useful for scripts
  # };
}
