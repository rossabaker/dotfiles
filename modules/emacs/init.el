;;; init.el --- Ross A. Baker's Emacs Configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Ross A. Baker

;; Author: Ross A. Baker <ross@rossabaker.com>
;; Keywords:

;;; Commentary:

;; Packages in this Emacs configuration are derived from
;; Nix. `used-packages.el' introspects this file and extracts
;; Nix packages from the `use-package' statements.
;;
;; Most dependencies are on MELPA.  Those that aren't appear in the
;; overlay in default.nix.
;;
;; If you like what you see, copy, as I have done from others.  If you
;; see room for improvement, pull requests are welcome.  This is my
;; personal config, and I might not merge it, but I always enjoy a
;; good editor discussion.

;;; Code:

;;;; use-package

(eval-when-compile
  (require 'use-package))

;;;; Basic human decency

;; I am a Chicago Bears fan.  They're still my team in the 2020s, even
;; if they make me think of 1985.  In that sense, they're like Emacs'
;; defaults.
(use-package better-defaults)

;;;; Keybindings

(use-package general)

(use-package which-key
  :config
  (which-key-mode +1))

;;;; Completion

(use-package ivy
  :config
  (ivy-mode +1))

(use-package counsel
  :config
  (counsel-mode +1))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode +1))

(use-package company
  :config
  (global-company-mode +1))

;;;; Search

(use-package swiper
  :general
  ([remap isearch-forward-regexp] 'swiper-isearch)
  ([remap isearch-backward-regexp] 'swiper-isearch-backward))

;; Necessary for projectile-ripgrep
(use-package ripgrep
  :defer t)

;;;; Version control

(use-package magit
  :config
  :general
  (:prefix "C-c g"
           "" '(nil :wk "git")
           "s" 'magit-status))

;;;; Projects

(use-package projectile
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1)
  ;; No grep. No ag. Only ripgrep.
  (define-key projectile-command-map "s" 'undefined)
  :general
  ("C-c p" '(:keymap projectile-command-map :wk "projectile"))
  (:keymaps 'projectile-command-map
            "s" 'projectile-ripgrep))

;;;; IDE

(use-package lsp-mode
  :preface
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package flycheck
  :config
  (global-flycheck-mode +1))

;;;; Whitespace

(use-package ws-butler
  :hook
  ((prog-mode text-mode) . ws-butler-mode))

;;;; Appearance

(use-package ansi-color
  :config
  (defun ross/colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :commands compile
  :hook (compilation-filter . ross/colorize-compilation-buffer))

(use-package modus-operandi-theme
  :after modus-vivendi-theme
  :config
  (defun ross/modus-themes-toggle ()
    "Toggle between `modus-operandi' and `modus-vivendi' themes."
    (interactive)
    (if (eq (car custom-enabled-themes) 'modus-operandi)
        (progn
          (disable-theme 'modus-operandi)
          (load-theme 'modus-vivendi t))
      (disable-theme 'modus-vivendi)
      (load-theme 'modus-operandi t)))
  (load-theme 'modus-operandi t)
  :general
  (:prefix "C-c T"
           "" '(nil :wk "theme")
           "t" '(ross/modus-themes-toggle :wk "modus-themes-toggle")))

(use-package modus-vivendi-theme)

;;;; Fin.

(provide 'init)

;;; init.el ends here
