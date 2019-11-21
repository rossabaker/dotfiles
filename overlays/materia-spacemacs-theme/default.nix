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
      ./change_color.sh -o Materia-Spacemacs-dark <(echo -e "BG=212026\\nFG=b2b2b2\\nSEL_BG=bc6ec5\\nMATERIA_SURFACE=212026\\nMATERIA_VIEW=292b2e\\nHDR_BG=222226\\nHDR_FG=b2b2b2\\nTERMINAL_COLOR4=4f97d7\\nTERMINAL_COLOR5=c56ec3\\nTERMINAL_COLOR9=e0211d\\nTERMINAL_COLOR11=dc752f\\nTERMINAL_COLOR12=86dc2f\\nINACTIVE_MATERIA_VIEW=292b2e")
    '';

    installPhase = ''
      mkdir -p "$out"/share/themes
      mv .themes/Materia-Spacemacs-dark "$out"/share/themes
    '';
  });
}
