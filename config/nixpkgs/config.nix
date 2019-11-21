{
  allowUnfree = true;

  overlays = [
    (import ../../overlays/arc-spacemacs-theme.nix)
    (import ../../overlays/materia-spacemacs-theme)
    (import ../../overlays/zoom-us.nix)
  ];
}
