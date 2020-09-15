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

;;;; Search

(use-package swiper
  :bind
  ([remap isearch-forward-regexp] . swiper-isearch)
  ([remap isearch-backward-regexp] . swiper-isearch-backward))

;;;; Version control

(use-package magit
  :config
  (which-key-add-key-based-replacements "C-c g" "git")
  :bind
  ("C-c g s" . magit-status))

;;;; Projects

(use-package projectile
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

;;;; Whitespace

(use-package ws-butler
  :hook
  ((prog-mode text-mode) . ws-butler-mode))

;;;; Fin.

(provide 'init)

;;; init.el ends here
