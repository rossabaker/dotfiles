{ pkgs, ... }:

let
  dpi = 192;
in
{
  imports = [
    (import ../../modules/home.nix { inherit dpi pkgs; })
    (import ../../modules/desktop.nix { inherit dpi pkgs; })
  ];

  programs = {
    autorandr = {
      profiles = {
        "mobile" = {
          fingerprint = {
            eDP1 = "00ffffffffffff0006afeb3200000000251b0104a5221378020925a5564f9b270c50540000000101010101010101010101010101010152d000a0f0703e803020350058c1100000180000000f0000000000000000000000000020000000fe0041554f0a202020202020202020000000fe00423135365a414e30332e32200a000d";
          };
          config = {
            eDP1 = {
              enable = true;
              dpi = dpi;
              mode = "3840x2160";
              position = "0x0";
              primary = true;
              rate = "60.00";
            };
          };
        };
      };
    };

    git.signing = {
      key = "904C153733DBB0106915C0BD975BE5BC29D92CA5";
      signByDefault = true;
    };
  };
}
