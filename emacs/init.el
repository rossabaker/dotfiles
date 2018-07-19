(require 'package)
;; optional. makes unpure packages archives unavailable
(setq package-archives nil)
(setq package-enable-at-startup nil)
(package-initialize)

(eval-when-compile
  (require 'use-package))

(use-package material-theme
  :ensure t
  :config
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (load-theme 'material t)))))

(use-package better-defaults)

(use-package desktop
  :config
  (desktop-save-mode t))

(use-package haskell-mode)

(use-package magit
  :bind ("C-c m" . magit-status))

(use-package nix-mode)

(use-package restart-emacs)
