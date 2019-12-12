{
  allowUnfree = true;

  overlays = [
    (import ../../emacs-overlay)
    (import ../../overlays/materia-tomorrow-theme)
    (import ../../overlays/qogir-tomorrow-theme)
    (import ../../overlays/zoom-us.nix)
  ];
}
