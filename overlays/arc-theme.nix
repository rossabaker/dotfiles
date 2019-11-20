self: super: {
  arc-spacemacs-theme = super.arc-theme.overrideAttrs (old: {
    pname = "arc-spacemacs-theme";
    nativeBuildInputs = old.nativeBuildInputs ++ [ super.bc ];

    preBuild = ''
      export HOME="$PWD"
      patchShebangs ./change_color.sh
      patchShebangs ./scripts/mix.sh
      patchShebangs ./scripts/darker.sh
      ./change_color.sh -a "--with-gnome-shell=${self.stdenv.lib.versions.majorMinor self.gnome3.gnome-shell.version}" -o Arc-Spacemacs-Dark <(echo -e "BG=292b2e\nFG=b2b2b2\nMENU_BG=212026\nMENU_FG=b2b2b2\nSEL_BG=444155\nBTN_BG=212026\nTXT_BG=292b2e\nSEL_FG=b2b2b2")
    '';

    installPhase = ''
      mkdir -p "$out"/share/themes
      mv .themes/Arc-Spacemacs-Dark "$out"/share/themes/Arc-Spacemacs-Dark
    '';
  });
}
