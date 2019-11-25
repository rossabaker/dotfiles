self: super: {
  materia-spacemacs-theme = super.materia-theme.overrideAttrs (old: rec {
    pname = "materia-spacemacs-theme";
    version = "c95281e81605876c817feb5f874c07fb8e28822d";

    nativeBuildInputs = old.nativeBuildInputs ++ [
      super.inkscape
      super.optipng
      super.parallel
      super.sassc
    ];

    src = self.fetchFromGitHub {
      owner = "nana-4";
      repo = "materia-theme";
      rev = "${version}";
      sha256 = "1d9f31xfv788xznmprsa29cyv83wwncrsfahw521qajlpzw8xayz";
    };

    patches = [ ./0001-Parameterize-inactive-colors.patch ];

    dontBuild = false;

    buildPhase = ''
      export HOME="$PWD"
      patchShebangs *.sh scripts src/gtk src/gtk-2.0
      ./change_color.sh -o Materia-Tomorrow-Night-dark <(echo -e "BG=222427\\nFG=c5c8c6\\nSEL_BG=b294bb\\nMATERIA_SURFACE=222427\\nMATERIA_VIEW=1d1f21\\nHDR_BG=222427\\nHDR_FG=c5c8c6\\nTERMINAL_COLOR4=8abeb7\\nTERMINAL_COLOR5=b294bb\\nTERMINAL_COLOR9=cc6666\\nTERMINAL_COLOR11=de935f\\nTERMINAL_COLOR12=8abeb7\\nINACTIVE_MATERIA_VIEW=222427")
    '';

    installPhase = ''
      mkdir -p "$out"/share/themes
      mv .themes/* "$out"/share/themes
    '';
  });
}
