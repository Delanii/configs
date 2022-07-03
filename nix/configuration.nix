# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
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

    #
    # Basic tools
    #
    coreutils-full
    wget
    gawk
    git

    #
    # File managers
    #
    (pkgs.callPackage /etc/nixos/pkgs/doublecmd/default.nix {})
    krusader
    vifm-full
    ranger

    #
    # Web browsers
    #
    chromium
    firefox
    opera
    tor-browser-bundle-bin
    nyxt

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
    # Advanced utilities
    #
    tmux
    wezterm

    #
    # Development tools
    #
    # Python
    python310 # current version in 14. 06. 2022 is 3.10.5
    conda

    # R
    R
    # rstudio # 14. 06. 2022 not building. Issue here: https://github.com/NixOS/nixpkgs/pull/177021

    # Julia
    julia-bin

    # Haskell
    # haskellPackages.ghcup # ghcup is marked as broken ... And it seems like none is interested to make ghcup run on nixos
    ghc
    cabal-install

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
    racket
    guile_3_0 # the current version in 14. 06. 2022

    # C programming and tools
    gnumake
    cmake
    gcc
    clang
    # llvm # Reportedly, `llvm` is part of `clang` already

    # Polyglot programming
    graalvm17-ce

    # TeXlive
    texlive.combined.scheme-full # I couldnt make work the manual TeXlive installation
    texworks

    #
    # More tools
    #
    # Preprocessors
    gpp

    # Make HTTP requests easy
    postman
    newman

    # Containers and emulation
    docker
    qemu
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

  # Test of a custom emacs overlay

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
  #      version = "29.0.50"; # Not accepted in the overlay, but the revision repository commit is actually for emacs master with version 29.0.50
         rev = "787c4ad8b0776280305a220d6669c956d9ed8a5d";
         sha256 = "FIefdqudf4Yp5QqchEZWDjGdjEbtSkd2Kp/O0LRFvAY=";
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
