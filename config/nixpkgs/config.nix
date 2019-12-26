let
  sources = import ../../nix/sources.nix;
in
{
  allowUnfree = true;

  overlays = [
    (import sources.emacs-overlay)
    (import ../../overlays/materia-tomorrow-theme)
    (import ../../overlays/qogir-tomorrow-theme)
    (import ../../overlays/zoom-us.nix)
  ];
}
