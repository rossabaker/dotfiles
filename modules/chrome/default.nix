{ pkgs, lib, ... }:

let
  apps = {
    gcalendar = "https://calendar.google.com/b/0/r";
    gmail = "https://mail.google.com/mail/u/0";
    work-gcalendar = "https://calendar.google.com/b/1/r";
    work-gmail = "https://mail.google.com/mail/u/1";
  };
  chrome-app = name: url: pkgs.writeScriptBin "${name}" ''
    #!${pkgs.stdenv.shell}
    exec ${pkgs.google-chrome}/bin/google-chrome-stable --app=${url}
  '';
in
{
  home.packages = [
    pkgs.google-chrome
  ] ++ lib.mapAttrsToList chrome-app apps;
}
