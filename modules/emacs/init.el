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

;;;; Personal

(validate-setq user-full-name "Ross A. Baker"
               user-mail-address "ross@rossabaker.com")

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

;;;; Security

(use-package auth-source
  :config
  (validate-setq auth-sources '("~/.authinfo.gpg")))

;;;; Keybindings

(use-package general)

(use-package which-key
  :general
  ("C-x C-z" nil) ;; C-z is sufficient, and even that only helps in tty
  (:prefix "C-c a"
           "" '(nil :wk "apps"))
  (:prefix "C-c a r"
           "" '(nil :wk "repls"))
  (:prefix "C-c d"
           "" '(nil :wk "directory"))
  (:prefix "C-c f"
           "" '(nil :wk "file"))
  (:prefix "C-c j"
           "" '(nil :wk "jump"))
  (:prefix "C-c x"
           "" '(nil :wk "text"))
  :config
  (which-key-mode +1))

(use-package quick-yes
  ;; The common approach is to reset `yes-or-no-p' to `y-or-n-p`, but
  ;; the distinction exists for a reason: `yes-or-no-p` is for weightier
  ;; questions.  This package preserves the distinction so we have to
  ;; stop and think, but allows a terser M-y and M-n to answer.
  )

;;;; Utilities

;; These are the kind of utilities we find in various starter kits and
;; Emacs distributions.  We don't need that weight, but some of them
;; are super useful!  Crux originated in Emacs Prelude.
(use-package crux
  ;;
  :general
  ("C-c f u" 'crux-view-url
   "C-x 4 t" 'crux-transpose-windows
   "C-c f k" 'crux-delete-file-and-buffer
   "C-c f c" 'crux-copy-file-preserve-attributes
   "C-c x d" 'crux-duplicate-current-line-or-region
   "C-c x D" 'crux-duplicate-and-comment-current-line-or-region
   "C-c f m" 'crux-rename-file-and-buffer
   "C-c f w" 'crux-kill-buffer-truename
   "M-o" 'crux-other-window-or-switch-buffer
   [remap move-beginning-of-line] 'crux-move-beginning-of-line
   "M-m" nil ;; obsolete with remapping of move-beginning-of-line
   [(shift return)] 'crux-smart-open-line
   [(control shift return)] 'crux-smart-open-line-above))

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
  ([remap recentf-open-files] 'counsel-recentf
   "C-c j o" 'counsel-outline
   "C-c f l" 'counsel-locate))

(use-package counsel-projectile
  :config
  ;; We flattened "s", so don't let counsel-projectile add more
  (validate-setq counsel-projectile-key-bindings
                 (assoc-delete-all "si" counsel-projectile-key-bindings #'string=))
  (counsel-projectile-mode +1))

(use-package company
  :config
  (setq company-show-numbers t)
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
  (validate-setq magit-clone-default-directory "~/src/"
                 magit-save-repository-buffers 'dontask)
  :general
  (:prefix "C-c g"
           "g" 'magit-dispatch
           "s" 'magit-status))

(use-package forge
  :after magit
  :general
  ("C-c g f" 'forge-dispatch))

(use-package git-gutter
  :demand t
  :config
  (validate-setq git-gutter:update-interval 1)
  (global-git-gutter-mode +1)
  :general
  (:prefix "C-c g h"
           "" '(nil :wk "hunk")
           "SPC" 'git-gutter:mark-hunk
           "n" 'git-gutter:previous-hunk
           "p" 'git-gutter:next-hunk
           "s" 'git-gutter:stage-hunk
           "r" 'git-gutter:revert-hunk))

(use-package git-gutter-fringe
  :demand t
  :after git-gutter
  :config
  (validate-setq git-gutter-fr:side 'left-fringe)
  (define-fringe-bitmap 'git-gutter-fr:added [240] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [240] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240 248 252 254 255] nil nil 'bottom)
  (setq-local git-gutter:init-function      #'git-gutter-fr:init
              git-gutter:view-diff-function #'git-gutter-fr:view-diff-infos
              git-gutter:clear-function     #'git-gutter-fr:clear
              git-gutter:window-width -1))

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

(use-package avy
  :general
  ("C-c j j" 'avy-goto-char-timer)
  ("C-c j l" 'avy-goto-line))

(use-package imenu
  :config
  (validate-setq imenu-auto-rescan t)
  :general
  ("M-i" 'imenu))

(use-package ace-window
  :config
  (defun ross/ace-window-or-switch-buffer ()
    ;; Like crux-other-window-or-switch-buffer, but with ace
    "Call `other-window' if more than one window is visible.
Switch to most recent buffer otherwise."
    (interactive)
    (if (one-window-p)
        (switch-to-buffer nil)
      (call-interactively 'ace-window)))
  (validate-setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :general
  ([remap other-window] 'ace-window
   "M-o" '(ross/ace-window-or-switch-buffer :wk ace-window-or-switch-buffer)))

;;;; IDE

(use-package lsp-mode
  :after direnv
  :preface
  (setq lsp-keymap-prefix "C-c l")
  ;; Experimental workaround to https://github.com/wbolster/emacs-direnv/issues/17,
  (advice-add 'lsp :before 'direnv-update-environment)
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
  (validate-setq flycheck-indication-mode 'right-fringe)
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    ;; Reverses the default double arrow for move to right fringe
    [#b00011011
     #b00110110
     #b01101100
     #b11011000
     #b01101100
     #b00110110
     #b00011011])
  (global-flycheck-mode +1))

(use-package direnv
  :demand t
  :config
  (validate-setq direnv-always-show-summary nil)
  (direnv-mode +1)
  :general
  (:prefix "C-c d e"
           "" '(nil :wk "direnv")
           "a" 'direnv-allow
           "e" 'direnv-update-environment
           "d" 'direnv-update-directory-environment))

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

;;;; Editing

(use-package emacs
  :after crux
  :config
  (crux-with-region-or-line kill-region))

(use-package ws-butler
  :hook
  ((prog-mode text-mode) . ws-butler-mode))

(use-package editorconfig
  ;; This is a polite default.
  :config
  (editorconfig-mode +1))

;;;;; Spelling

(use-package flyspell
  :hook
  (prog-mode . flyspell-prog-mode)
  (text-mode . flyspell-mode))

(use-package flyspell-correct
  :after flyspell
  :general
  (:keymaps 'flyspell-mode-map
            "C-;" 'flyspell-correct-wrapper))

(use-package flyspell-correct-ivy
  :after flyspell-correct)

;;;; History

(use-package recentf
  :config
  (recentf-mode +1)
  :general
  ("C-c f r" 'recentf-open-files))

(use-package savehist
  :config
  (validate-setq savehist-save-minibuffer-history t
                 savehist-additional-variables '(kill-ring
                                                 mark-ring global-mark-ring
                                                 search-ring regexp-search-ring)))

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
  (defmacro ross/modus-themes-format-sexp (sexp &rest objects)
    `(eval (read (format ,(format "%S" sexp) ,@objects))))
  (dolist (theme '("operandi" "vivendi"))
    (ross/modus-themes-format-sexp
     (defun ross/modus-%1$s-load-theme ()
       (let ((colors modus-%1$s-theme-default-colors-alist))
         (load-theme 'modus-%1$s t)
         (set-face-attribute 'git-gutter-fr:added nil
                             :background (cdr (assoc "bg-main" colors))
                             :foreground (cdr (assoc "green-fringe-bg" colors)))
         (set-face-attribute 'git-gutter-fr:modified nil
                             :background (cdr (assoc "bg-main" colors))
                             :foreground (cdr (assoc "yellow-fringe-bg" colors)))
         (set-face-attribute 'git-gutter-fr:deleted nil
                             :background (cdr (assoc "bg-main" colors))
                             :foreground (cdr (assoc "red-fringe-bg" colors)))))
     theme))
  (defun ross/modus-themes-toggle ()
    "Toggle between `modus-operandi' and `modus-vivendi' themes."
    (interactive)
    (if (eq (car custom-enabled-themes) 'modus-operandi)
        (progn
          (disable-theme 'modus-operandi)
          (ross/modus-vivendi-load-theme))
      (disable-theme 'modus-vivendi)
      (ross/modus-operandi-load-theme)))
  (ross/modus-operandi-load-theme)
  :general
  (:prefix "C-c T"
           "" '(nil :wk "theme")
           "t" '(ross/modus-themes-toggle :wk "modus-themes-toggle")))

(use-package modus-vivendi-theme)

(use-package hl-line
  :config
  (global-hl-line-mode +1))

(use-package hl-todo
  :config
  (global-hl-todo-mode +1))

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

(use-package rainbow-delimiters
  :config
  ;; Show them off
  (ignore '((((((((())))))))))
  :hook
  (prog-mode . rainbow-delimiters-mode))

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

;;;; Interpreters

(defvar ross/interpreters '()
  "An alist of known interpreters for various languages.")
(defvar ross/run-interpreter-history '()
  "History for ross/run-interpreter.")
(defun ross/run-interpreter ()
  "Run an interpreter."
  (interactive)
  (let ((sel (completing-read "Run interpreter: "
                              ross/interpreters nil t
                              nil 'ross/run-interpreter-history)))
    (funcall (cdr (assoc sel ross/interpreters)))))
(general-define-key
 "C-c a i" '(ross/run-interpreter :wk "run-interpreter"))

;;;; Languages

;;;;; Bash

(defun ross/run-bash ()
  "Run `bash' in an inferior process."
  (interactive)
  (let ((explicit-shell-file-name "bash"))
    (shell (get-buffer-create "*bash*"))))
(add-to-list 'ross/interpreters '("Bash" . ross/run-bash))

;;;;; Haskell

(use-package haskell-mode
  :config
  (validate-setq haskell-process-log t
                 haskell-process-show-debug-tips nil
                 haskell-process-suggest-remove-import-lines t
                 haskell-process-suggest-hoogle-imports t)
  ;; This one grinds everything to a halt.
  (setq-default flycheck-disabled-checkers '(haskell-stack-ghc))
  :hook
  (haskell-mode . interactive-haskell-mode)
  (haskell-mode . subword-mode)
  (haskell-cabal-mode . subword-mode)
  :general
  (:keymaps 'haskell-mode-map
            :prefix "C-c m"
            "" '(nil :wk "haskell")
            "h" 'hoogle
            "t" 'haskell-session-change-target)
  (:keymaps 'haskell-mode-map
            :prefix "C-c m g"
            "" '(nil :wk "go")
            "i" 'haskell-navigate-imports
            "I" 'haskell-navigate-imports-return)
  (:keymaps 'haskell-mode-map
            :prefix "C-c m r"
            "" '(nil :wk "interactive")
            "r" 'haskell-interactive-bring))

(use-package lsp-haskell
 :config
 (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper")
 :hook
 (haskell-mode . lsp))

(use-package ormolu
  :after crux
  :config
  (crux-with-region-or-buffer ormolu-format-region)
  :general
  (:keymaps 'haskell-mode-map
            "C-c m f" 'ormolu-format-region))

;;;;; Nix

;; Nix: the cause of, and solution to, all of life's problems.

(use-package nix-mode
  :init
  (add-to-list 'ross/interpreters '("Nix" . nix-repl))
  :config
  (defun ross/nix-repl-fix-echoes ()
    "Fix redundant echo of input in the Nix REPL."
    (setq comint-process-echoes t))
  :hook
  (nix-mode . subword-mode)
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

(use-package python
  :init
  (add-to-list 'ross/interpreters '("Python" . run-python)))

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

;;;;; Rust

(use-package rustic
  :hook
  (rust-mode . subword-mode)
  :general
  (:keymaps 'rustic-mode-map
            :prefix "C-c m"
            "" '(nil :wk "rust")
            "f" 'rustic-format-buffer))

;;;;; Scala

(use-package scala-mode
  :general
  (:keymaps 'scala-mode-map
            :prefix "C-m"
            "" '(nil :wk "scala"))
  :hook
  (scala-mode . lsp)
  (scala-mode . subword-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :general
  (:keymaps 'scala-mode-map
            :prefix "C-m b"
            "" '(nil :wk "sbt")
            "b" 'sbt-start))

(use-package lsp-metals)

;;;; Formats

;;;;; JSON

(use-package json-mode)

(use-package jq-format
  :after crux
  :config
  (crux-with-region-or-buffer jq-format-json-region)
  :general
  (:keymaps 'json-mode-map
            "C-c m f" 'jq-format-json-region))

;;;;; YAML

(use-package yaml-mode)

(use-package yaml-imenu
  :config
  (yaml-imenu-enable))

;;;; Other tools

;;;;; Docker

(use-package dockerfile-mode)

;;;; Fin.

(provide 'init)

;;; init.el ends here
