(require 'package)
;; optional. makes unpure packages archives unavailable
(setq package-archives nil)
(setq package-enable-at-startup nil)
(package-initialize)

(eval-when-compile
  (require 'use-package))

(use-package better-defaults)

(use-package desktop
  :config
  (desktop-save-mode t))

(use-package haskell-mode)


