;;; init.el --- Ross A. Baker's Emacs configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; All packages in my Emacs are declared in nix.  Packages that
;; require configuration have their own use-package expressions below.

;;; Code:

;;;; Package management

(eval-when-compile
  (setq use-package-enable-imenu-support t)
  (require 'use-package))

;;;; Better defaults

(use-package better-defaults)
(use-package no-littering)

;;;; Core

(use-package emacs
  :custom
  (inhibit-startup-echo-area-message user-login-name)
  (inhibit-startup-screen t)
  (initial-scratch-message ""))

;;;;; Security

(use-package "auth-source"
  :custom
  ;; Emacs stores `authinfo' in $HOME and in plain-text. Let's not do that,
  ;; mkay?  This file stores usernames, passwords, and other such treasures for
  ;; the aspiring malicious third party.
  ;; h/t Doom core
  (auth-sources (list (concat user-emacs-directory "authinfo.gpg")
                      "~/.authinfo.gpg")))

(use-package "gnutls"
  ;; Emacs is essentially one huge security vulnerability, what with all the
  ;; dependencies it pulls in from all corners of the globe. Let's try to be at
  ;; least a little more discerning.
  ;; h/t Doom core
  :custom
  (gnutls-verify-error (not (getenv "INSECURE")))
  (gnutls-algorithm-priority
   (when (boundp 'libgnutls-version)
     (concat "SECURE128:+SECURE192:-VERS-ALL"
             (if (>= libgnutls-version 30605)
                 ":+VERS-TLS1.3")
             ":+VERS-TLS1.2")))
  (;; `gnutls-min-prime-bits' is set based on recommendations from
   ;; https://www.keylength.com/en/4/
   gnutls-min-prime-bits 3072))

;;;;; Garbage collection

(use-package gcmh
  :demand t)

;;;; UI

(use-package emacs
  :config
;;;;; Cursor
  (setq cursor-in-non-selected-windows nil)
  (blink-cursor-mode -1)

;;;;; Scrolling
  (setq fast-but-imprecise-scrolling t)
  (setq hscroll-step 1)
  (setq scroll-conservatively 101)
  (setq scroll-margin 3)
  (setq scroll-preserve-screen-position t)

;;;;; Windows and frames
  (setq frame-inhibit-implied-resize t)
  (setq frame-resize-pixelwise t)
  (setq window-resize-pixelwise t)
  (setq use-dialog-box nil)

;;;;; Beep beep your ass
  ;; h/t doom-themes
  (defun ross/visual-bell-fn ()
    "Blink the mode-line with the error face. Set `ring-bell-function' to this to use it."
    (let ((bell-cookie (face-remap-add-relative 'mode-line 'error)))
      (force-mode-line-update)
      (run-with-timer 0.15 nil
                      (lambda (cookie buf)
                        (with-current-buffer buf
                          (face-remap-remove-relative cookie)
                          (force-mode-line-update)))
                      bell-cookie
                      (current-buffer))))
  (setq ring-bell-function 'ross/visual-bell-fn)

;;;;; Minibuffer
  (setq enable-recursive-minibuffers t)
  (setq echo-keystrokes 0.02)
  ;; Try really hard to keep the cursor from getting stuck in the read-only prompt
  ;; portion of the minibuffer.
  ;; h/t Doom core-ui
  (setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;;;;; Packages

(use-package all-the-icons)

(use-package "comint"
  :custom
  (comint-prompt-read-only t)
  (comint-scroll-to-bottom-on-input 'this)
  (comint-move-point-for-output 'others)
  (comint-scroll-show-maximum-output t)

  :config
  ;; h/t Doom core-ui
  (defun ross/apply-ansi-color-to-compilation-buffer-h ()
  "Applies ansi codes to the compilation buffers. Meant for
`compilation-filter-hook'."
  (with-silent-modifications
    (ansi-color-apply-on-region compilation-filter-start (point))))
  :hook (compilation-filter . ross/apply-ansi-color-to-compilation-buffer-h))

(use-package "compile"
  :custom
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-scroll-output 'first-error))

(use-package "display-line-numbers"
  :custom
  (display-line-numbers-width 4)
  (display-line-numbers-widen t)
  :hook
  ((conf-mode prog-mode text-mode) . display-line-numbers-mode))

(use-package "frame"
  :config
  (set-face-attribute 'default nil
                      :family "Hasklig"
                      :height 90
                      :weight 'normal
                      :width 'normal))

(use-package "hl-line"
  :custom
  (hl-line-sticky-flag nil)
  :config
  ;; h/t Doom core-ui
  (defvar ross/inhibit-hl-line nil)
  (defun ross/activate-mark-h ()
    (when hl-line-mode
      (setq-local ross/inhibit-hl-line t)
      (hl-line-mode -1)))
  (defun ross/deactivate-mark-h ()
    (when ross/inhibit-hl-line
      (hl-line-mode +1)))
  :hook
  ((conf-mode prog-mode text-mode) . hl-line-mode)
  ;; Inspired by doom: temporarily disable when the mark is set
  (activate-mark . ross/activate-mark-h)
  (deactivate-mark . ross/deactivate-mark-h))

(use-package quick-yes
  ;; yes-or-no-p exists for a reason: it's for things that require more care than
  ;; y-or-n-p, but it's still obnoxious.  This package is a compromise: it
  ;; supports M-y and M-n
  )

(use-package rainbow-delimiters
  :custom
  (rainbow-delimiters-max-face-count 4)
  :hook
  (prog-mode . rainbow-delimiters-mode))

;;;;; Themes

(use-package modus-vivendi-theme
  :custom
  (modus-vivendi-theme-slanted-constructs t)
  (modus-vivendi-theme-faint-syntax t)
  (modus-vivendi-theme-prompts 'subtle)
  (modus-vivendi-theme-completions 'opinionated)
  (modus-vivendi-theme-fringes nil)
  (modus-vivendi-theme-intense-paren-match t)
  (modus-vivendi-theme-diffs 'desaturated)
  :config
  (load-theme 'modus-vivendi t))

;;;; Unorganized territory

;; We shall endeavor to keep everything out of this, but sometimes
;; Emacs really wants to dump custom settings itself.
(when (file-exists-p custom-file)
  (load custom-file))

(use-package ace-window
  :bind
  ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package "autorevert"
  :config
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t))

(use-package "apropos"
  :bind
  ("C-h a" . 'apropos))

(use-package avy
  :demand t ;; So theme can change its face
  :bind
  ("C-'" . avy-goto-char-timer)
  :config
  (setq avy-style 'at-full
        avy-background t)
  (avy-setup-default))

(use-package bazel-mode
  :mode "BUILD\\'")

(use-package beacon
  :bind
  ("C-c b" . beacon-blink)
  :config
  (beacon-mode +1)
  (add-to-list 'beacon-dont-blink-major-modes 'shell-mode)
  (add-to-list 'beacon-dont-blink-major-modes 'sbt-mode))

(use-package beginend
  :config
  (beginend-global-mode))

(use-package "company"
  :hook
  (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-show-numbers t
        company-tooltip-align-annotations t
        company-global-modes '(not shell-mode)))

(use-package company)

(use-package company-lsp
  :after company lsp-mode
  :config
  (push 'company-lsp company-backends))

(use-package company-quickhelp
  :bind
  (:map company-active-map
        ("M-h" . company-quickhelp-manual-begin))
  :config
  (setq company-quickhelp-delay nil)
  (company-quickhelp-mode +1))

(use-package company-restclient
  :config
  (push 'company-restclient company-backends))

(use-package copy-as-format
  :bind
  ("C-c w g" . copy-as-format-github)
  ("C-c w h" . copy-as-format-html)
  ("C-c w j" . copy-as-format-jira)
  ("C-c w m" . copy-as-format-markdown)
  ("C-c w s" . copy-as-format-slack)
  ("C-c w o" . copy-as-format-org-mode))

(use-package counsel
  :bind
  ("C-c c" . counsel-compile)
  ("C-c f d" . counsel-dired)
  ("C-c f l" . counsel-locate)
  ("C-c f r" . counsel-recentf)
  ("C-h F" . counsel-describe-face)
  ("C-h l" . counsel-find-library)
  ("C-h i" . counsel-info-lookup-symbol)
  ("C-x 8 RET" . counsel-unicode-char)
  ("C-x C-f" . counsel-find-file)
  ("M-i" . counsel-imenu)
  ("M-x" . counsel-M-x)
  ("M-y" . counsel-yank-pop))

(use-package counsel-jq)

(use-package counsel-projectile
  :config
  (counsel-projectile-mode)
  :bind
  ("C-c s p" . counsel-projectile-rg))

(use-package crux
  :bind
  ("C-k" . crux-smart-kill-line)
  ("C-c I" . crux-find-user-init-file)
  ("C-c d" . crux-duplicate-current-line-or-region)
  ;;("C-c e" . crux-eval-and-replace)
  ("C-c f k" . crux-delete-file-and-buffer)
  ("C-c f m" . crux-rename-file-and-buffer)
  ("C-x 4 t" . crux-transpose-windows))

(use-package "css-mode"
  :mode "\\.rasi\\'")

(use-package deadgrep
  :bind
  ("C-c s d" . deadgrep))

(use-package "delsel"
  :config
  (delete-selection-mode t))

(use-package "descr-text"
  :bind
  ("C-h '" . describe-char))

(use-package dhall-mode
  :config
  (setq dhall-format-arguments '("--ascii")))

(use-package "dired"
  :config
  (setq dired-dwim-target t))

(use-package direnv
  :demand t
  :config
  (direnv-mode)
  (setq direnv-always-show-summary nil)
  :hook
  (eshell-directory-change . direnv-update-directory-environment))

(use-package doom-modeline
  :ensure t
  :config
  (defun ross/set-modeline-heights (frame)
    (dolist (face '(mode-line mode-line-inactive))
      (set-face-attribute face nil :height 80)))
  (add-hook 'after-make-frame-functions 'ross/set-modeline-heights)
  (setq doom-modeline-buffer-encoding nil
        doom-modeline-height 28
        doom-modeline-icon (or (display-graphic-p) (daemonp)))
  (doom-modeline-mode 1))

(use-package dockerfile-mode)

(use-package dtrt-indent
  :hook
  (prog-mode . dtrt-indent-mode))

(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'ivy))

(use-package "eldoc")

(use-package electric-operator
  :hook
  (scala-mode . electric-operator-mode)
  :config
  (apply #'electric-operator-add-rules-for-mode 'scala-mode
         (electric-operator-get-rules-for-mode 'prog-mode))
  (electric-operator-add-rules-for-mode 'scala-mode
                                        (cons "<-" " <- ")
                                        (cons "->" " -> ")
                                        (cons "=>" " => ")
                                        (cons "<:" " <: ")
                                        (cons ":>" " :> ")
                                        (cons "<%" " <% ") ;; deprecated
                                        (cons "%%" " %% ")
                                        (cons "%%%" " %%% ")
                                        (cons "/*" " /* ")
                                        (cons "//" " // ")
                                        (cons "++" " ++ ")
                                        (cons "++=" " ++= ")
                                        ;; Cats operators
                                        (cons "*>" " *> ")
                                        (cons "<*" " <* ")
                                        (cons "===" " === ")
                                        (cons "=!=" " =!= ")
                                        (cons ">>=" " >>= ")
                                        (cons ">>" " >> ")
                                        (cons "|-|" " |-| ")
                                        (cons "|+|" " |+| ")
                                        (cons "<+>" " <+> ")
                                        (cons "<+>" " <+> ")
                                        (cons "<<<" " <<< ")
                                        (cons ">>>" " >>> ")
                                        (cons "&&&" " &&& ")
                                        (cons "-<" " -< ")
                                        (cons "~>" " ~> ")
                                        (cons ":<:" " :<: ")
                                        (cons "&>" " &> ")
                                        (cons "<&" " <& ")
                                        (cons "<<" " << ")
                                        ;; sbt operators
                                        (cons ":=" " := ")))

(use-package emacs
  :config
  (setq create-lockfiles nil
        user-full-name "Ross A. Baker"
        user-mail-address "ross@rossabaker.com")
  (setq-default cursor-type 'bar
                fill-column 80)
  (put 'narrow-to-region 'disabled nil)

  ;; scroll-margin is irritating in modes where the focus tends to be the bottom
  (defun ross/unset-scroll-margin ()
    "Set \"scroll-margin\" to zero."
    (setq-local scroll-margin 0))
  (dolist (hook '(messages-buffer-mode-hook comint-mode-hook term-mode-hook))
    (remove-hook hook 'ross/unset-scroll-margin)
    (add-hook hook 'ross/unset-scroll-margin))
  :bind
  ("C-x k" . kill-this-buffer)
  ("C-x K" . kill-buffer))

(use-package ethan-wspace
  :config
  (global-ethan-wspace-mode))

(use-package ess
  :init (require 'ess-site)
  :commands R)

(use-package "executable"
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package exec-path-from-shell
  :config
  ;; My systemd service has stopped reading the path.
  (exec-path-from-shell-initialize))

(use-package expand-region
  :bind
  ("C-=" . 'er/expand-region))

(use-package "files"
  :init
  (setq mode-require-final-newline nil
        require-final-newline nil)
  :config
  (defun ross/guess-mode ()
    "Guess mode of file in `fundamental-mode'."
    (interactive)
    (and (eq major-mode 'fundamental-mode) (set-auto-mode)))
  (unbind-key "C-x C-d") ;; list-directory is silly
  :bind
  ("C-c f u" . recover-this-file)
  :hook
  (after-save . ross/guess-mode))

(use-package flycheck
  :config
  (global-flycheck-mode)
  (setq flycheck-global-modes
        '(not haskell-interactive-mode ;; https://github.com/haskell/haskell-mode/issues/1015
              )))

(use-package "flyspell"
  :config
  (setq ispell-program-name "aspell")
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode))

(use-package git-gutter
  :config
  (global-git-gutter-mode +1)
  (setq git-gutter:update-interval 1))

(use-package git-link
  :config
  (setq git-link-use-commit t)
  :bind
  ("C-c g l l" . git-link)
  ("C-c g l c" . git-link-commit)
  ("C-c g l h" . git-link-homepage))

(use-package git-timemachine
  :bind
  ("C-c g t" . git-timemachine-toggle))

(use-package gitconfig-mode)

(use-package gitignore-mode)

(use-package "goto-addr"
  :hook
  (text-mode . goto-address-mode)
  (prog-mode . goto-address-prog-mode))

(use-package goto-line-faster)

(use-package haskell-mode
  :hook (haskell-mode . interactive-haskell-mode)
  :config
  (setq
   ;; TODO This should not be global, but it lets us build with cabal
   ;; inside a nix shell.
   haskell-process-type 'cabal-new-repl
   haskell-process-suggest-remove-import-lines t
   haskell-process-auto-import-loaded-modules t
   haskell-process-log t
   haskell-process-wrapper-function #'identity))

(use-package hasklig-mode
  :hook
  haskell-mode
  scala-mode)

(use-package "help"
  :config
  (unbind-key "C-h g") ;; I already have a religion, thanks.
  )

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-comand)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol))

(use-package hydra)

(use-package "help"
  :after which-key
  :config
  (global-unset-key (kbd "C-h C-h")) ; Undo conflict with which-key help
  :bind
  ("C-h b b" . describe-bindings))

(use-package iedit
  :bind
  ("C-c s e" . iedit-mode))

(use-package "imenu"
  :hook
  (imenu-after-jump . ross/recenter-top))

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1))

(use-package ivy-rich
  :config
  (ivy-rich-mode +1))

(use-package jinja2-mode
  :mode "\\.ede\\'")

(use-package json-mode)

(use-package list-environment)

(use-package lsp-haskell
  :demand
  :config
  (setq lsp-haskell-process-path-hie "ghcide"
        lsp-haskell-process-args-hie '())
  :hook
  (haskell-mode . lsp))

(use-package lsp-metals
  :after lsp-mode
  :demand t)

(use-package lsp-mode
  :hook
  (scala-mode . lsp)
  :config
  (setq lsp-enable-snippet nil
        lsp-prefer-flymake nil))

(use-package lsp-treemacs
  :bind
  ("C-c e t" . lsp-treemacs-errors-list))

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-enable nil))

(use-package magit
  :bind
  ("C-c g g" . magit-dispatch)
  ("C-c g S" . magit-stage-file)
  ("C-c g s" . magit-status)
  ("C-c g U" . magit-unstage-file))

(use-package multi-line
  :bind
  ("C-c l m" . multi-line)
  ("C-c l s" . multi-line-single-line))

(use-package multi-vterm
  :bind
  ("C-c t 2" . multi-vterm)
  ("C-c t n" . multi-vterm-next)
  ("C-c t p" . multi-vterm-prev)
  ("C-c t t" . multi-vterm-dedicated-toggle)
  ("C-c t P" . multi-vterm-projectile))

(use-package mwim
  :bind
  ([remap move-beginning-of-line] . mwim-beginning)
  ([remap move-end-of-line] . mwim-end))

(use-package nixpkgs-fmt
  :bind (:map nix-mode-map
              ("C-c m f" . nixpkgs-fmt-buffer)))

(use-package nix-mode
  :config
  (defun ross/nix-mode-comint-hook ()
    (setq comint-process-echoes t))
  :hook
  (nix-repl-mode . ross/nix-mode-comint-hook)
  :bind
  ("C-c a n" . nix-repl))

(use-package nix-sandbox)

(use-package "outline"
  :hook
  (emacs-lisp-mode . outline-minor-mode))

(use-package "ns-win"
  :if (eq system-type 'darwin)
  :config
  (when
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'super)))

(use-package "package"
  :config
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")))

(use-package page-break-lines
  :config
  (global-page-break-lines-mode))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default)
  (defun ross/pop-to-scratch ()
    (interactive)
    (pop-to-buffer "*scratch*"))
  :bind
  ("C-c f x" . ross/pop-to-scratch))

(use-package "proced"
  :hook
  (proced-mode . (lambda () (proced-toggle-auto-update +1)))
  :config
  (setq proced-auto-update-interval 1))

(use-package "prog-mode"
  :config
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (global-prettify-symbols-mode +1))

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (if (file-directory-p "~/src")
      (setq projectile-project-search-path '("~/src")))
  (setq projectile-completion-system 'ivy)
  (projectile-discover-projects-in-search-path)
  (let ((cmd "fd . --color=never --type f -0 -H -E .git"))
    (setq projectile-generic-command cmd
          projectile-git-command cmd)))

(use-package protobuf-mode)

(use-package rainbow-mode
  :hook
  (prog-mode . rainbow-mode))

(use-package "recentf"
  :commands counsel-recentf recentf-open-files
  :config
  (setq recentf-max-menu-items 0
        recentf-max-saved-items 100)
  (defun ross/recentf-add-directory ()
    "Add directory to recentf file list."
    (recentf-add-file default-directory))
  :hook
  (dired-mode . ross/recentf-add-directory))

(use-package restart-emacs)

(use-package restclient
  :mode
  ("\\.restclient\\'" . restclient-mode))

(use-package ripgrep)

(use-package "savehist"
  :config
  (setq savehist-additional-variables '(kill-ring
                                        search-ring
                                        regexp-search-ring
                                        last-kbd-macro
                                        kmacro-ring
                                        shell-command-history))
  (savehist-mode +1))

(use-package sbt-mode
  :commands
  sbt-start
  sbt-command
  :bind
  (:map sbt:mode-map ("C-a" . comint-bol))
  :config
  (add-to-list 'sbt:program-options "-Dsbt.supershell=false"))

(use-package scala-mode
  :config
  (subword-mode +1)
  (which-key-declare-prefixes-for-mode 'scala-mode "C-c m" "scala")
  :bind
  ("C-c m b" . sbt-hydra)
  ("C-c m c" . sbt-do-compile)
  ("C-c m t" . sbt-do-test))

(use-package shell-pop
  :init
  (setq shell-pop-shell-type '("shell" "*shell*" (lambda () (shell)))
        shell-pop-universal-key "C-c t s"))

(use-package "simple"
  :config
  (column-number-mode +1)
  (size-indication-mode +1)
  (defun ross/kill-ring-file-name-save ()
    "Yank the current file name"
    (interactive)
    (let ((filename (if (equal major-mode 'dired-mode)
                        default-directory
                      (buffer-file-name))))
      (when filename
        (kill-new filename)
        (message "File name \"%s\" saved to the kill ring" filename))))
  (setq kill-do-not-save-duplicates t
        kill-whole-line t
        read-quoted-char-radix 16)
  :hook
  (text-mode . visual-line-mode)
  :bind
  ("C-c f w" . ross/kill-ring-file-name-save)
  ([remap capitailize-word] . capitalize-dwim)
  ([remap downcase-word] . downcase-dwim)
  ([remap upcase-word] . upcase-dwim))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)
  (smartparens-global-mode +1)
  (show-smartparens-global-mode +1)
  (defun ross/smartparens-for-eval-expression ()
    "Enable \"smartparens-mode\" in the minibuffer during \"eval-expression\"."
    (when (eq this-command 'eval-expression)
      (smartparens-mode +1)))
  :hook
  (minibuffer-setup . ross/smartparens-for-eval-expression))

(use-package so-long
  :config
  (global-so-long-mode))

(use-package stan-mode)

(use-package string-inflection
  :bind
  ("C-c q !" . string-inflection-upcase)
  ("C-c q C" . string-inflection-camelcase)
  ("C-c q S" . string-inflection-capital-underscore)
  ("C-c q c" . string-inflection-lower-camelcase)
  ("C-c q k" . string-inflection-kebab-case)
  ("C-c q q" . string-inflection-all-cycle)
  ("C-c q s" . string-inflection-underscore))

(use-package "subword")

(use-package sudo-edit
  :config
  (sudo-edit-indicator-mode)
  :bind
  ("C-c f s" . sudo-edit))

(use-package swiper
  :bind
  ("C-c C-r" . ivy-resume)
  ("C-r" . swiper-isearch-backward)
  ("C-s" . swiper-isearch))

(use-package systemd
  :mode ("\\.service"))

(use-package title-capitalization)

(use-package "tooltip"
  :config
  (tooltip-mode -1)
  (setq tooltip-use-echo-area t))

(use-package try)

(use-package "uniquify"
  :config
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package unfill
  :bind
  ("M-q" . unfill-toggle))

(use-package visual-regexp
  :bind
  ("C-c s r r" . vr/replace)
  ("C-c s r q" . vr/query-replace))

(use-package vterm)

(use-package which-key
  :demand t
  :init
  (global-unset-key (kbd "C-h b"))
  :config
  (which-key-mode)
  (which-key-declare-prefixes
    "C-c f" "files"
    "C-c g" "git"
    "C-c l" "multi-line"
    "C-c p" "projectile"
    "C-c q" "string-inflection"
    "C-c s" "search"
    "C-c t" "terminals"
    "C-c T" "theme"
    "C-h b" "bindings")
  :bind
  ("C-h b f" . which-key-show-full-keymap)
  ("C-h b i" . which-key-show-minor-mode)
  ("C-h b k" . which-key-show-keymap)
  ("C-h b m" . which-key-show-major-mode)
  ("C-h b t" . which-key-show-top-level))

(use-package "window"
  :config
  (defun ross/recenter-top ()
    "Place point scroll-margin lines from the top of the window."
    (recenter scroll-margin)))

(use-package "woman"
  :bind
  ("C-h r" . woman) ; rtfm
  )

(use-package ws-butler
  :hook
  (prog-mode . ws-butler-mode))

(use-package yaml-mode)

;; NixOS doesn't have a #!/bin/bash. We do this a lot.
(defun ross/fix-shebang ()
  "Replace #!/bin/bash shebang with #!/usr/bin/env bash"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "#!/bin/bash\n")
      (delete-region (point) (line-end-position))
      (insert "#!/usr/bin/env bash"))))


;;; init.el --- Ross A. Baker's Emacs configuration.

(provide 'init)

;;; init.el ends here
