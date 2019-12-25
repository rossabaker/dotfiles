self: super: {
  qogir-tomorrow-theme = super.qogir-theme.overrideAttrs (old: rec {
    pname = "qogir-tomorrow-theme";

    buildInputs = old.buildInputs ++ [
      self.sassc
      self.which # to find sassc
    ];

    buildPhase = ''
      export HOME="$PWD"
      echo "WTF"
      echo "$PATH"
      echo "WTF"
      patchShebangs *.sh src/*/*.sh
      ./parse-sass.sh
    '';

    patches = [ ./0001-Tomorrow-theme-colors.patch ];
  });
}
