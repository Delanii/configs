# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      # ./unstable.nix # 2023-07-28: vale is alreadz available in the nixos-stable channel
      # <home-manager/nixos> # add the home-manager channel to the nixos configuration; this cannot be used when the home-manager is set in a `flake.nix` file
      # ../home-manager/home.nix # This is useful when home-manager configuration is stored in a different file (see home-manager configuration example close to the end of the file). But flakes require a different setup.
    ];

  # Bootloader.
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sda";
  boot.loader.grub.useOSProber = true;

  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Prague";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.utf8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "cs_CZ.utf8";
    LC_IDENTIFICATION = "cs_CZ.utf8";
    LC_MEASUREMENT = "cs_CZ.utf8";
    LC_MONETARY = "cs_CZ.utf8";
    LC_NAME = "cs_CZ.utf8";
    LC_NUMERIC = "cs_CZ.utf8";
    LC_PAPER = "cs_CZ.utf8";
    LC_TELEPHONE = "cs_CZ.utf8";
    LC_TIME = "cs_CZ.utf8";
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the KDE Plasma Desktop Environment.
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;

  # Configure keymap in X11
  services.xserver = {
    layout = "cz";
    xkbVariant = "";
  };

  # Configure console keymap
  console.keyMap = "cz-lat2";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.tomaskrulis = {
    isNormalUser = true;
    description = "tomaskrulis";
    extraGroups = [ "networkmanager" "wheel" ];
  };

  # Enable automatic login for the user.
  services.xserver.displayManager.autoLogin.enable = true;
  services.xserver.displayManager.autoLogin.user = "tomaskrulis";

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Setup graphics drivers
  # hardware.nvidia.modesetting.enable = true; # in case of issues, see wiki here: https://nixos.wiki/wiki/Nvidia ; follow the page to get full Prime offload setup
  services.xserver.videoDrivers = [ "nvidia" ]; # this was tested on a VM, so better not play with this on VM

  # List packages installed in system profile. To search, run:
  # $ nix search wget

  environment.systemPackages = with pkgs; [
    #
    # Text editors
    #
    (emacs.override {
      nativeComp = true;
      withPgtk = true;
      withSQLite3 = true;
      withCsrc = true;
      withXinput2 = true;
      withImageMagick = true;
    })
    vim
    neovim
    vscode
    kate

    # Fonts
    fira-code
    fira-code-symbols

    #
    # Basic tools
    #
    coreutils-full
    moreutils
    wget
    gawk
    git
    (ripgrep.override {withPCRE2 = true;})
    flex
    rlwrap
    gnuplot

    # Shells
    starship
    nushell
    babashka

    #
    # File managers
    #
    # (pkgs.callPackage /etc/nixos/pkgs/doublecmd/default.nix {}) # non-flake config
    (pkgs.callPackage ../custom-derivations/doublecmd/default.nix {})
    krusader
    broot
    vifm-full
    ranger
    doublecmd

    #
    # Web browsers
    #
    chromium
    firefox
    opera
    tor-browser-bundle-bin
    nyxt
    lynx
    w3m
    elinks

    #
    # Media players
    #
    vlc
    mpv
    mplayer

    #
    # Gaming
    #
    steam
    # steam-run
    lutris
    # steam-runtime -- seems to be unavailable. Test further if it becomes required for whatever reason.
    wine-staging
    mono
    vulkan-tools

    #
    # Communication
    #
    discord

    #
    # Document readers
    #
    libsForQt5.okular
    calibre

    #
    # Data synchronization
    #
    megasync
    syncthing
    croc

    #
    # Downloaders
    #
    obs-studio # screen-recording
    # jdownloader # not in nixpkgs repo
    qbittorrent

    #
    # Graphics
    #
    inkscape
    gimp

    #
    # Advanced utilities
    #
    # Terminals and terminal multiplexers
    alacritty
    tmux
    wezterm
    zellij

    # Document generation and conversion
    haskellPackages.pandoc-cli # worth for an overlay
    # haskellPackages.pandoc_2_18 # 31. 07. 2022: doesnt build
    # pandoc-crossref # as of 13. 07. 2022 is not in nixpkgs stable repo
    shellcheck
    hugo
    haskellPackages.hakyll
    codebraid # worth for an overlay

    # Text expanders
    espanso # worth for an overlay

    # Search everywhere
    ripgrep-all

    # Working with video
    ffmpeg

    # PDF tools
    poppler

    # OCR -- optical content recognition
    tesseract5

    # Working with pictures
    imagemagick

    # Spellcheck, prose linting
    aspell
    hunspell
    hunspellDicts.cs_CZ
    hunspellDicts.en_US-large
    # grammarly # 13. 07. 2022 not packaged for nixos
    vale # set to retrieve from unstable -- I want v. 2.20.0 for org-mode support
    tabnine

    # Containers and emulation
    docker
    podman
    qemu
    genymotion # android emulation
    virtualbox
    distrobox

    # Preprocessors
    gpp
    dhall
    dhall-nix
    dhall-json
    dhall-docs

    # Parser generators
    tree-sitter
    antlr

    # More finders
    fd
    fzf
    skim
    navi

    # Format processors
    fq # binary processor
    yq # yaml processor
    jq # JSON processor
    htmlq
    jc # JSON-convert
    dasel
    libxml2
    xmlstarlet
    xidel
    saxon-he
    libxslt

    # CSV and databases
    pgcli
    miller
    xsv

    # Make HTTP requests easy
    postman
    newman
    xh
    httpie

    # Keyboard programming
    # haskellPackages.kmonad # Flagged as broken sinnce 30. 07. 2022

    # Emails
    mutt
    neomutt

    # Various
    lsd
    delta
    procs
    bat
    zoxide
    sd
    exa

    #
    # Development tools
    #
    # Python
    python312 # current version in 14. 06. 2022 is 3.10.5
    conda

    # R
    R
    rstudio

    # Julia
    julia

    # Haskell
    # haskellPackages.ghcup # ghcup is marked as broken ... And it seems like noone is interested to make ghcup run on nixos
    ghc
    cabal-install
    stack

    # Rust
    rustup

    # Clojure
    clojure
    leiningen

    # Common lisp
    sbcl
    roswell

    # Go
    go

    # Lua
    lua5_4
    luajit

    # Scheme
    guile_3_0 # the current version in 14. 06. 2022

    # C programming and tools
    gnumake
    cmake
    gcc
    clang
    # llvm # Reportedly, `llvm` is part of `clang` already
    gnu-cobol
    gnuapl
    gforth

    # Forth
    pforth

    # Standard ML
    mlton
    mlkit

    # Proofs and logic
    swiProlog
    gprolog

    # Various programming languages for fun
    scala
    erlang
    # dyalog # installable probably from here: https://github.com/markus1189/dyalog-nixos
    ocaml
    postgresql
	haskellPackages.squeal-postgresql
    fpc
	haxe
	vlang
	nim
	zig
	j

    # Polyglot programming
    # graalvm17-ce

    # TeXlive
    texlive.combined.scheme-full # I couldnt make work the manual TeXlive installation
    texworks
    ghostscript
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };
  
  # Add steam
  programs.steam.enable = true;
  hardware.opengl.enable = true; # for vulan drivers setup
  hardware.opengl.driSupport32Bit = true; # to allow running wine with 32-bit games

  # Package overlays declaration

  nixpkgs.overlays = [

  # Crazy, recompile everything, Gentoo-style:

   # (final: prev: {
   #    stdenv = prev.stdenvAdapters.addAttrsToDerivation {
   #      NIX_CFLAGS_COMPILE = "-pipe -march=native -O3 -ffloat-store -fexcess-precision=style -ffast-math -fno-rounding-math -fno-signaling-nans -fcx-limited-range -fno-math-errno -funsafe-math-optimizations -fassociative-math -freciprocal-math -ffinite-math-only -fno-signed-zeros -fno-trapping-math -frounding-math -fsingle-precision-constant -fcx-fortran-rules";
   #    } prev.stdenv;
   # })

   (self: super:{
     emacs = super.emacs.overrideAttrs (old: {
       NIX_CFLAGS_COMPILE = "-O3 -mtune=native -march=native -fomit-frame-pointer";
       src = super.fetchFromSavannah {
         repo = "emacs";
  #      version = "29.1"; # Not accepted in the overlay, but the revision repository commit is actually for emacs master with version 29.1
         rev = "9c12c3b7c59ee102d3a022368ea050fc9e3bb186";
         sha256 = "sha256-E7+8KyQ4K3xVylHovKqvPU8GhoNvU//tAW/WQ9a9ORY=";
       };
       configureFlags = (old.configureFlags or []) ++ [
         "--with-modules"
       ];
     });
   })

  (self: super: {
    discord = super.discord.overrideAttrs (
      _: { src = builtins.fetchTarball {
        url = "https://discord.com/api/download?platform=linux&format=tar.gz";
        sha256 = "1bhjalv1c0yxqdra4gr22r31wirykhng0zglaasrxc41n0sjwx0m";
        };
      }
    );
  })

  ];

  # # Home-manager settings
  # #
  # # I have left here these settings to have an example of the syntax construct if I would want to put the home-manager configuration back into this file.
  # #
  # # These settings can be moved into a separate file (nix module) and imported in the `imports` list. If these settings are in a different file, the code
  # #
  # # { config, pkgs, ... }: {
  # # #   home-manager settings here
  # # }
  # #
  # # has to be added to the file.
  # #
  # home-manager.users.tomaskrulis = { pkgs, ... }: {
  #   home.packages = with pkgs; [
  #     alacritty
  #   ];
  #   # Test of writing specific configuration files with home-manager
  #   home.file = { # $HOME path is prepended before the file path. There is no need to write `~/`
  #     "nix-write-file-test/test.txt".text =
  #       ''
  #       This is a test of { special } symbols # and this too \ ...
  #       '';
  #   };
  #   home.file.".doom.d" = { # this could be used for .doom.d for example
  #     source = /home/tomaskrulis/Documents/MegaSynchonized/MegaSynchronized/configs/emacs/DoomEmacs/.doom.d;
  #     recursive = true;
  #     # executable = true; # useful for scripts
  #   };
  # };

  nix = {
    package = pkgs.nixFlakes;
    extraOptions = "experimental-features = nix-command flakes";
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leavecatenate(variables, "bootdev", bootdev)
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?

}
