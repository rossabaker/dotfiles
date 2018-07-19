{ stdenv, fetchurl, dpkg, makeWrapper
, alsaLib, atk, cairo, cups, curl, dbus, expat, fontconfig, freetype, glib
, gnome2, gtk3, libnotify, libxcb, nspr, nss, systemd, xorg }:

let

  version = "4.1.0";

  rpath = stdenv.lib.makeLibraryPath [
    alsaLib
    atk
    cairo
    cups
    curl
    dbus
    expat
    fontconfig
    freetype
    glib
    gnome2.GConf
    gnome2.gdk_pixbuf
    gnome2.pango
    gtk3
    libnotify
    libxcb
    nspr
    nss
    stdenv.cc.cc
    systemd

    xorg.libxkbfile
    xorg.libX11
    xorg.libXcomposite
    xorg.libXcursor
    xorg.libXdamage
    xorg.libXext
    xorg.libXfixes
    xorg.libXi
    xorg.libXrandr
    xorg.libXrender
    xorg.libXtst
    xorg.libXScrnSaver
  ] + ":${stdenv.cc.cc.lib}/lib64";

  src =
    if stdenv.system == "x86_64-linux" then
      fetchurl {
        url = "https://update.gitter.im/linux64/gitter_4.1.0_amd64.deb";
        sha512 = "2ea0ebbc05cbdc40e4e8cb07a68b3948477a61a81e550b8f131af46315c6fc7547c0954aa101260f93b63c557f3e00b1e23006a236b77be01ea4a82edd170f70";
      }
    else
      throw "Gitter is not supported on ${stdenv.system}";

in stdenv.mkDerivation {
  name = "gitter-${version}";

  inherit src;

  dontBuild = true;
  buildInputs = [ dpkg makeWrapper ];
  
  unpackPhase = ''
    dpkg -x $src .
  '';
  
  installPhase = ''
    mkdir -p $out

    cp -r ./opt/Gitter/linux64 $out

    ls -lR $out

    patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" "$out/linux64/Gitter"
    makeWrapper "$out/linux64/Gitter" "$out/bin/gitter" \
      --prefix LD_LIBRARY_PATH : "${rpath}"
  '';

  meta = with stdenv.lib; {
    description = "Desktop client for Gitter";
    homepage = https://gitter.im;
    license = licenses.mit;
    platforms = [ "x86_64-linux" ];
  };
}
