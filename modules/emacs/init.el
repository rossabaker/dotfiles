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
(use-package no-littering
  :config
  ;; These are litter, too.
  (validate-setq create-lockfiles nil))

;;;; Keybindings

(use-package general)

(use-package which-key
  :general
  (:prefix "C-c a"
           "" '(nil :wk "apps"))
  (:prefix "C-c a r"
           "" '(nil :wk "repls"))
  :config
  (which-key-mode +1))

;;;; Completion

(use-package ivy
  :config
  (ivy-mode +1))

(use-package ivy-xref
  :config
  (validate-setq xref-show-definitions-function #'ivy-xref-show-defs
                 xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package counsel
  :config
  (counsel-mode +1)
  :general
  ([remap recentf-open-files] 'counsel-recentf))

(use-package counsel-projectile
  :config
  ;; We flattened "s", so don't let counsel-projectile add more
  (validate-setq counsel-projectile-key-bindings
                 (assoc-delete-all "si" counsel-projectile-key-bindings #'string=))
  (counsel-projectile-mode +1))

(use-package company
  :config
  (global-company-mode +1))

(use-package company-box
  :hook
  (company-mode . company-box-mode))

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

;;;; Navigation

(use-package emacs
  :general
  (:prefix "C-c f"
           "" '(nil :wk "file"))
  ("M-o" 'other-window))

(use-package recentf
  :config
  (recentf-mode +1)
  :general
  ("C-c f r" 'recentf-open-files))

(use-package imenu
  :general
  ("M-i" 'imenu))

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

;;;; Languages

;;;;; Nix

;; Nix: the cause of, and solution to, all of life's problems.

(use-package nix-mode
  :config
  (defun ross/nix-repl-fix-echoes ()
    "Fix redundant echo of input in the Nix REPL."
    (setq comint-process-echoes t))
  :general
  ("C-c a r n" 'nix-repl)
  :hook
  (nix-repl-mode . ross/nix-repl-fix-echoes))

(use-package nixpkgs-fmt
  :config
  (defun ross/nixpkgs-fmt-dwim ()
    (interactive)
    (call-interactively
     (if (region-active-p)
         'nixpkgs-fmt-region
       'nixpkgs-fmt-buffer)))
  :general
  (:keymap 'nix-mode-map
           "C-c c f" '(ross/nixpkgs-fmt-dwim :wk nixpkgs-fmt)))

;;;;; Python

;; My son was showing me Python turtle.  I tried it in Emacs.  He
;; installed Emacs of his own volition.  I guess now I have to learn a
;; little bit of Python.

(use-package python-mode
  :general
  ("C-c a r p" 'run-python))

;;;;;; Language servers

;;;;;;; Palantir's pyls.

;; Fuck ICE, and fuck Palantir.

;;;;;;; Microsoft's python-language-server

;; It kind of sort of works, and its corporate sponsor is moderately
;; less complicit in caging children.  But it looks like it's on the
;; way out: https://github.com/microsoft/python-language-server/issues/2096

(use-package lsp-python-ms
  :disabled t
  :config
  (defun ross/lsp-python-ms ()
    (require 'lsp-python-ms)
    (lsp-deferred))
  ;; Nix packaging calls it `python-language-server'.
  (validate-setq lsp-python-ms-executable (executable-find "python-language-server"))
  :hook
  (python-mode . ross/lsp-python-ms))

;;;;;;; Microsoft's pylance

;; This is the future of Microsoft's efforts, but is only licensed for
;; use with Microsoft's editors.  This is an ominous development in
;; the Era of LSP, but whatever.

;;;;;;; Microsoft's pyright

;; This underlies pylance, and is fully open source.  It's hard to say
;; how much intelligence is upstream in pylance, but this works pretty
;; well out of the box!

(use-package lsp-pyright
  :config
  (defun ross/lsp-pyright ()
    (require 'lsp-pyright)
    (lsp-deferred))

  ;; Irritation: the lsp-find-references are also returning *.pyi
  ;; stubs.  The type hints we get from them are already in the docs,
  ;; and otherwise, they just seem to get in the way of jumping to the
  ;; source.
  (defun ross/xref-python-stub-p (item)
    "Return t if `item' is from a *.pyi stub."
    (string-suffix-p ".pyi" (oref (xref-item-location item) file)))

  (defun ross/filter-not-python-stub-definitions (items)
    "Remove Python stubs from a list of xref-items."
    (cl-remove-if #'ross/xref-python-stub-p items))

  (advice-add 'lsp--locations-to-xref-items
              :filter-return #'ross/filter-not-python-stub-definitions)

  :hook
  (python-mode . ross/lsp-pyright))

;;;;;;; lsp-jedi

;; Still the default in VSCode.  Installation on NixOS seems fraught, so
;; putting this one on hold.

;;;; Fin.

(provide 'init)

;;; init.el ends here
