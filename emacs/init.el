(eval-when-compile
  (require 'use-package))

(use-package base16-theme
  :config
  (load-theme 'base16-materia t))

(use-package better-defaults
  :config
  ;; better-defaults sets one worse default
  (ido-mode -1))
