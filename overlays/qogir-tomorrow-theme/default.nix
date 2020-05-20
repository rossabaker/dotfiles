self: super: let
  pin = self.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channels";
    rev = "8686922e68dfce2786722acad9593ad392297188";
    sha256 = "1pc92s1nbr9hlsmzlf8w2pc90rlma649y3fvyfww0sbcwn0lb65n";
  };
  pinnedPkgs = import pin {};
in {
  qogir-tomorrow-theme = super.qogir-theme.overrideAttrs (
    old: rec {
      pname = "qogir-tomorrow-theme";

      buildInputs = old.buildInputs ++ [
        pinnedPkgs.sassc # infinite loop starting after 3.6.1
        self.which # to find sassc
      ];

      buildPhase = ''
        export HOME="$PWD"
        patchShebangs *.sh src/*/*.sh
        ./parse-sass.sh
      '';

      patches = [ ./0001-Tomorrow-theme-colors.patch ];
    }
  );
}
