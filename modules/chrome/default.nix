{ pkgs, lib, ... }:

let
  chrome = pkgs.google-chrome;
  extensions = {
    youtube-tv = "nlmaamaoahjiilibgbafebhafkeccjac";
  };
in {
  programs.chromium.extensions = lib.attrValues extensions;

  home.packages = [
    chrome
  ] ++ lib.mapAttrsToList (name: id:
    pkgs.writeScriptBin name ''
      #!${pkgs.stdenv.shell}
      ${chrome}/bin/google-chrome-stable --app-id=${id}
    ''
  ) extensions;
}
