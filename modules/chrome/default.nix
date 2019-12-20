{ pkgs, lib, ... }:

let
  chrome-profile = "Default";
  extensions = {
    gcalendar = "kjbdgfilnfhdoflbpgamdcdgpehopbep";
    gmail = "pjkljhegncpnkpknbcohdijeoejaedia";
    youtube-tv = "nlmaamaoahjiilibgbafebhafkeccjac";
  };
  chrome = pkgs.writeScriptBin "chrome" ''
    #!${pkgs.stdenv.shell}
    exec ${pkgs.google-chrome}/bin/google-chrome-stable --profile-directory="${chrome-profile}" "$@"
  '';
  chrome-app = name: id: pkgs.writeScriptBin "work-${name}" ''
    #!${pkgs.stdenv.shell}
    exec ${chrome}/bin/chrome --app-id=${id}
  '';
in {
  programs.chromium.extensions = lib.attrValues extensions;

  home.packages = [
    chrome
  ] ++ lib.mapAttrsToList (name: id:
    pkgs.writeScriptBin name ''
      #!${pkgs.stdenv.shell}
      ${chrome}/bin/chrome --app-id=${id}
    ''
  ) extensions;
}
