;;; init.el --- Ross A. Baker's Emacs Configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Ross A. Baker

;; Author: Ross A. Baker <ross@rossabaker.com>
;; Keywords:

;;; Commentary:

;; Packages in this Emacs configuration are derived from
;; Nix.  `used-packages.el' introspects this file and extracts
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

;;;; Bootstrap

(eval-and-compile
  (setq use-package-enable-imenu-support t))
(eval-when-compile
  (require 'use-package))

;; Provides validate-setq, which we'll use extensively whilst configuring other
;; packages.
(use-package validate)

;;;; Basic human decency

;; I am a Chicago Bears fan.  They're still my team in the 2020s, even
;; if they make me think of 1985.  In that sense, they're like Emacs'
;; defaults.
(use-package better-defaults)

;; I can't teach my kids to pick up after themselves, but I can teach
;; my Emacs to.
(use-package no-littering)

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
  ;; We flattened "s", so don't let counsel-projectile add more
  (validate-setq counsel-projectile-key-bindings
                 (assoc-delete-all "si" counsel-projectile-key-bindings #'string=))
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

(general-define-key
 :prefix "C-c g"
 "" '(nil :wk "git"))

(use-package magit
  :config
  :general
  (:prefix "C-c g"
           "s" 'magit-status))

(use-package git-gutter
  :config
  (setq git-gutter:update-interval 1)
  (global-git-gutter-mode +1)
  :general
  (:prefix "C-c g h"
           "" '(nil :wk "hunk")
           "SPC" 'git-gutter:mark-hunk
           "n" 'git-gutter:previous-hunk
           "p" 'git-gutter:next-hunk
           "s" 'git-gutter:stage-hunk
           "r" 'git-gutter:revert-hunk))

;;;; Projects

(use-package projectile
  :config
  (validate-setq projectile-completion-system 'ivy)
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

(use-package display-line-numbers-mode
  :hook
  ((prog-mode conf-mode) . display-line-numbers-mode))

(use-package imenu
  :general
  ("M-i" 'imenu))

(use-package try
  ;; A downside of a Nix-managed Emacs is that new packages require a
  ;; restart. The reproducibility is generally worth it, but sometimes
  ;; we just want to fix an unfamiliar language or try something we
  ;; read on Emacs News.
  :config
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (defun ross/package-refresh-contents-maybe ()
    "Refresh the packages if we have no `package-archive-contents'."
    (unless package-archive-contents
      (package-refresh-contents)))
  (advice-add 'try :before 'ross/package-refresh-contents-maybe)
  :general
  (:prefix "C-c P"
           "" '(nil :wk "package")
           "t" 'try))

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
  :demand
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

(use-package doom-modeline
  :config
  (progn ;; Borrowed from doom-themes
    (defface ross/visual-bell '((t (:inherit error)))
      "Face to use for the mode-line on visual-bell."
      :group 'ross/themes)
    (defun ross/themes-visual-bell-fn ()
      "Blink the mode-line briefly. Set `ring-bell-function' to this to use it."
      (let ((ross/bell-cookie (face-remap-add-relative 'mode-line 'ross/visual-bell)))
        (force-mode-line-update)
        (run-with-timer 0.15 nil
                        (lambda (cookie buf)
                          (with-current-buffer buf
                            (face-remap-remove-relative cookie)
                            (force-mode-line-update)))
                        ross/bell-cookie
                        (current-buffer))))
    (validate-setq ring-bell-function #'ross/themes-visual-bell-fn
                   visible-bell t))
  (doom-modeline-mode +1)
  (column-number-mode +1)
  (size-indication-mode +1))

;;;; Docs

(use-package helpful
  :config
  (validate-setq counsel-describe-function-function #'helpful-callable
                 counsel-describe-symbol-function #'helpful-symbol
                 counsel-describe-variable-function #'helpful-variable
                 counsel-descbinds-function #'helpful-function)
  :general
  (:keymaps 'help-map
            "h" 'helpful-at-point)
  ([remap describe-key] #'helpful-key))

;;;; Fin.

(provide 'init)

;;; init.el ends here
