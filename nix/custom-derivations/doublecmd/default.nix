{ stdenv
, lib
, fetchFromGitHub
, lazarus
, fpc
, qt5
, getopt
, libqt5pas
, glib
, dbus
, libX11
}:

stdenv.mkDerivation rec {
  pname = "doublecmd";
  version = "1.0.6";

  src = fetchFromGitHub {
    owner = "doublecmd";
    repo = "doublecmd";
    rev = "v${version}";
    sha256 = "sha256-aEWu/bRVOwjK6QTWsMntRYwAfjuwo9SNuH4qkQn0mOY=";
  };

  nativeBuildInputs = [
    lazarus
    fpc
    qt5.wrapQtAppsHook
    getopt
  ];

  buildInputs = [
    libqt5pas
    glib
    dbus
    libX11
  ];

  postPatch = ''
    patchShebangs build.sh install/linux/install.sh
    substituteInPlace build.sh \
      --replace '$(which lazbuild)' '"${lazarus}/bin/lazbuild --lazarusdir=${lazarus}/share/lazarus"'
    substituteInPlace install/linux/install.sh \
      --replace '$DC_INSTALL_PREFIX/usr' '$DC_INSTALL_PREFIX'
  '';

  buildPhase = ''
    runHook preBuild

    export HOME=$(mktemp -d)
    ./build.sh release qt5

    runHook postBuild
  '';

  NIX_LDFLAGS = "--as-needed -rpath ${lib.makeLibraryPath buildInputs}";

  installPhase = ''
    runHook preInstall

    # install -Dm755 {,$out/bin/}doublecmd
    install/linux/install.sh -I $out

    runHook postInstall
  '';
}
