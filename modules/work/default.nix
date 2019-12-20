{ pkgs, lib, ... }:

let
  chrome-profile = "Profile 1";
  extensions = {
    gcalendar = "kjbdgfilnfhdoflbpgamdcdgpehopbep";
    gmail = "pjkljhegncpnkpknbcohdijeoejaedia";
  };
  chrome = pkgs.writeScriptBin "work-chrome" ''
    #!${pkgs.stdenv.shell}
    exec ${pkgs.google-chrome}/bin/google-chrome-stable --profile-directory="${chrome-profile}" "$@"
  '';
  chrome-app = name: id: pkgs.writeScriptBin "work-${name}" ''
    #!${pkgs.stdenv.shell}
    exec ${pkgs.google-chrome}/bin/google-chrome-stable --app-id=${id} --profile-directory="${chrome-profile}"
  '';
in {
  home.packages = [
    chrome
    pkgs.zoom-us
  ] ++ lib.mapAttrsToList chrome-app extensions;
}
