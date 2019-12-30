;;; init.el --- Ross A. Baker's Emacs configuration.

;;; Commentary:

;; All packages in my Emacs are declared in nix.  Packages that
;; require configuration have their own use-package expressions below.

;;; Code:

(eval-when-compile
  (setq use-package-enable-imenu-support t)
  (require 'use-package))
(require 'delight)

(setq cursor-in-non-selected-windows nil
      hscroll-step 1
      inhibit-startup-screen t
      initial-scratch-message ""
      load-prefer-newer t
      scroll-conservatively 101
      scroll-preserve-screen-position t
      user-full-name "Ross A. Baker"
      user-mail-address "ross@rossabaker.com"
      visible-bell t)
(setq-default indent-tabs-mode nil)
(put 'narrow-to-region 'disabled nil)

;; We shall endeavor to keep everything out of this, but sometimes
;; Emacs really wants to dump custom settings itself.
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(use-package ace-window
  :delight
  :bind
  ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package "autorevert"
  :delight auto-revert-mode
  :config
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t))

(use-package "apropos"
  :config
  (setq apropos-do-all t))

(use-package atomic-chrome
  :config
  (atomic-chrome-start-server))

(use-package avy
  :bind
  ("C-'" . avy-goto-char-timer)
  :config
  (setq avy-style 'at-full)
  (avy-setup-default))

(use-package bazel-mode
  :mode "BUILD\\'")

(use-package beacon
  :delight
  :bind
  ("C-c b" . beacon-blink)
  :config
  (beacon-mode +1)
  (add-to-list 'beacon-dont-blink-major-modes 'shell-mode)
  (add-to-list 'beacon-dont-blink-major-modes 'sbt-mode))

(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-night t))

(use-package "company"
  :delight
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

(use-package counsel
  :bind
  ("C-c /" . counsel-git-grep)
  ("C-c c" . counsel-compile)
  ("C-c f d" . counsel-dired)
  ("C-c f l" . counsel-locate)
  ("C-c f r" . counsel-recentf)
  ("C-h f" . counsel-describe-function)
  ("C-h v" . counsel-describe-variable)
  ("C-h l" . counsel-find-library)
  ("C-h i" . counsel-info-lookup-symbol)
  ("C-x 8 RET" . counsel-unicode-char)
  ("C-x C-f" . counsel-find-file)
  ("M-x" . counsel-M-x)
  ("M-y" . counsel-yank-pop))

(use-package counsel-jq)

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

(use-package crux
  :bind
  ("C-a" . crux-move-beginning-of-line)
  ("C-k" . crux-smart-kill-line)
  ("C-c D" . crux-delete-file-and-buffer)
  ("C-c I" . crux-find-user-init-file)
  ("C-c d" . crux-duplicate-current-line-or-region)
  ;;("C-c e" . crux-eval-and-replace)
  ("C-c r" . crux-rename-file-and-buffer)
  ("C-x 4 t" . crux-transpose-windows))

(use-package "css-mode"
  :mode "\\.rasi\\'")

(use-package "delsel"
  :config
  (delete-selection-mode t))

(use-package "descr-text"
  :bind
  ("C-h '" . describe-char))

(use-package "desktop"
  :disabled t
  ;; This causes metals to spin up multiple JVMs when we restart
  ;; Emacs, and the daemon doesn't restore frames.  Maybe recentf is
  ;; good enough for now.
  :config
  (desktop-save-mode t))

(use-package dhall-mode
  :config
  (setq dhall-format-arguments '("--ascii")))

(use-package "dired"
  :config
  (setq dired-dwim-target t))

(use-package direnv
  :config
  (direnv-mode)
  :hook
  (eshell-directory-change . direnv-update-directory-environment))

(use-package "display-line-numbers"
  :hook
  (prog-mode . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-width 4
                display-line-numbers-widen t))

(use-package dockerfile-mode)

(use-package dtrt-indent
  :delight
  :hook
  (prog-mode . dtrt-indent-mode))

(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'ivy))

(use-package "ediff-wind"
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package "eldoc"
  :delight)

(use-package electric-operator
  :delight
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

(use-package ess
  :init (require 'ess-site)
  :commands R)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package "executable"
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package expand-region
  :bind
  ("C-=" . 'er/expand-region))

(use-package "files"
  :config
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
        require-final-newline t)
  :bind
  ("C-c f u" . recover-this-file))

(use-package flycheck
  :delight
  :config
  (global-flycheck-mode)
  (setq flycheck-global-modes
        '(not haskell-interactive-mode ;; https://github.com/haskell/haskell-mode/issues/1015
              )))

(use-package "flyspell"
  :delight
  :config
  (setq ispell-program-name "aspell")
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode))

(use-package "frame"
  :config
  (blink-cursor-mode -1)
  (setq default-frame-alist '((font . "Hasklig 12"))))

(use-package git-gutter
  :delight
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

(use-package haskell-mode
  :delight interactive-haskell-mode
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
  :delight
  :hook
  haskell-mode
  scala-mode)

(use-package "hippie-exp"
  :bind
  ([remap dabbrev-expand] . hippie-expand))

(use-package hydra)

(use-package "help"
  :after which-key
  :config
  (global-unset-key (kbd "C-h C-h")) ; Undo conflict with which-key help
  :bind
  ("C-h b b" . describe-bindings))

(use-package "ibuffer"
  :bind
  ([remap list-buffers] . ibuffer))

(use-package "imenu"
  :bind
  ("M-i" . imenu))

(use-package ivy
  :delight
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1))

(use-package ivy-rich
  :config
  (ivy-rich-mode +1))

(use-package json-mode)

(use-package list-environment)

(use-package lsp-haskell
  :demand
  :config
  (setq lsp-haskell-process-wrapper-function
        (lambda (args)
          (append
           (append (list "nix-shell" "-I" "." "--command" )
                   (list (mapconcat 'identity args " ")))
           (list (nix-current-sandbox)))))
  :hook
  (haskell-mode . lsp))

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

(use-package "menu-bar"
  :config
  (menu-bar-mode -1))

(use-package "mouse"
  :config
  (setq mouse-yank-at-point t))

(use-package multi-line
  :bind
  ("C-c l m" . multi-line)
  ("C-c l s" . multi-line-single-line))

(use-package nix-mode
  :config
  (defun ross/nix-mode-comint-hook ()
    (setq comint-process-echoes t))
  :hook
  (nix-repl-mode . ross/nix-mode-comint-hook)
  :bind
  ("C-c a n" . nix-repl))

(use-package nix-sandbox)

(use-package "ns-win"
  :if (eq system-type 'darwin)
  :config
  (when
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'super)))

(use-package "package"
  :config
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")))

(use-package "paren"
  :config
  (show-paren-mode 1))

(use-package page-break-lines
  :delight
  :config
  (global-page-break-lines-mode))

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
  (projectile-discover-projects-in-search-path))

(use-package protobuf-mode)

(use-package quick-yes)

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :delight
  :hook
  (prog-mode . rainbow-mode))

(use-package restart-emacs)

(use-package restclient
  :mode
  ("\\.restclient\\'" . restclient-mode))

(use-package "savehist"
  :config
  (setq savehist-additiona-variables '(kill-ring
                                       search-ring
                                       regexp-search-ring
                                       last-kbd-macro
                                       kmacro-ring
                                       shell-command-history))
  (savehist-mode +1))

(use-package "saveplace"
  :config
  (save-place-mode t))

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

(use-package "scroll-bar"
  :config
  ;; Disable stubborn scroll bars in emacsclient
  ;; https://emacs.stackexchange.com/a/46632
  (customize-set-variable 'scroll-bar-mode nil)
  (customize-set-variable 'horizontal-scroll-bar-mode nil))

(use-package shell-pop
  :init
  (setq shell-pop-shell-type '("shell" "*shell*" (lambda () (shell)))
        shell-pop-universal-key "C-c t"))

(use-package "simple"
  :delight visual-line-mode
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
  (setq save-interprogram-paste-before-kill t)
  :hook
  (text-mode . visual-line-mode)
  :bind
  ("C-c f w" . ross/kill-ring-file-name-save))

(use-package smartparens
  :delight
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)
  (smartparens-global-mode +1)
  (show-smartparens-global-mode +1))

(use-package snow
  :commands let-it-snow)

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

(use-package "subword"
  :delight)

(use-package swiper
  :bind
  ("C-c C-r" . ivy-resume)
  ("C-r" . swiper-isearch-backward)
  ("C-s" . swiper-isearch))

(use-package systemd
  :mode ("\\.service"))

(use-package title-capitalization)

(use-package "tool-bar"
  :config
  (tool-bar-mode -1))

(use-package "tooltip"
  :config
  (tooltip-mode -1)
  (setq tooltip-use-echo-area t))

(use-package try)

(use-package "uniquify"
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-ignore-buffers-re "^\\*"))

(use-package unfill
  :bind
  ("M-q" . unfill-toggle))

(use-package vterm)

(use-package which-key
  :delight
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
    "C-c T" "theme"
    "C-h b" "bindings")
  :bind
  ("C-h b f" . which-key-show-full-keymap)
  ("C-h b i" . which-key-show-minor-mode)
  ("C-h b k" . which-key-show-keymap)
  ("C-h b m" . which-key-show-major-mode)
  ("C-h b t" . which-key-show-top-level))

(use-package "woman"
  :bind
  ("C-h r" . woman) ; rtfm
  )

(use-package ws-butler
  :delight
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
