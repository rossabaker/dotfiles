;;; init.el --- Ross A. Baker's Emacs configuration.

;;; Commentary:

;; All packages in my Emacs are declared in nix.  Packages that
;; require configuration have their own use-package expressions below.

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'delight)

(setq inhibit-startup-screen t
      initial-scratch-message "")
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

(use-package atomic-chrome
  :config
  (atomic-chrome-start-server))

(use-package avy
  :bind
  ("C-'" . avy-goto-char-timer)
  :config
  (setq avy-style 'at-full)
  (avy-setup-default))

(use-package base16-theme
  :disabled t ;; Switched to sanityinc-tomorrow
  :config
  (load-theme 'base16-materia t)
  (set-face-attribute 'line-number-current-line nil
                      :background (plist-get base16-materia-colors :base01)
                      :foreground (plist-get base16-materia-colors :base04)
                      :inverse-video nil))

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

(use-package better-defaults
  :config
  ;; better-defaults sets one worse default
  (ido-mode -1))

(use-package color-theme-sanityinc-tomorrow
  :config
  (defun ross:load-theme-tomorrow-night ()
    (interactive)
    (load-theme 'sanityinc-tomorrow-night t nil))
  (defun ross:load-theme-tomorrow-day ()
    (interactive)
    (load-theme 'sanityinc-tomorrow-day t nil))
  :bind
  ("C-c T n" . ross:load-theme-tomorrow-night)
  ("C-c T d" . ross:load-theme-tomorrow-day))

(use-package company
  :delight
  :hook
  (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-show-numbers t
        company-tooltip-align-annotations t))

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
  ("C-c f l" . counsel-locate)
  ("C-c f r" . counsel-recentf)
  ("C-h f" . counsel-describe-function)
  ("C-h v" . counsel-describe-variable)
  ("C-h l" . counsel-find-library)
  ("C-h i" . counsel-info-lookup-symbol)
  ("C-x 8 RET" . counsel-unicode-char)
  ("C-x C-f" . counsel-find-file)
  ("M-x" . counsel-M-x))

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

(use-package desktop
  :disabled t
  ;; This causes metals to spin up multiple JVMs when we restart
  ;; Emacs, and the daemon doesn't restore frames.  Maybe recentf is
  ;; good enough for now.
  :config
  (desktop-save-mode t))

(use-package direnv
  :config
  (direnv-mode))

(use-package display-line-numbers
  :hook
  (prog-mode . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-width 4
                display-line-numbers-widen t))

(use-package dtrt-indent
  :delight
  :hook
  (prog-mode . dtrt-indent-mode))

(use-package eldoc
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

(use-package executable
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package expand-region
  :bind
  ("C-=" . 'er/expand-region))

(use-package flycheck
  :delight
  :config
  (global-flycheck-mode))

(use-package frame
  :config
  (blink-cursor-mode -1)
  ;; Hasklig is in home.packages, but on Mac, I needed to install it
  ;; into FontBook from ~/.nix-profile/share/fonts. ¯\_(ツ)_/¯
  (if (member "Hasklig" (font-family-list))
      (setq default-frame-alist '((font . "Hasklig-12")))))

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

(use-package hasklig-mode
  :delight
  :hook
  haskell-mode
  scala-mode)

(use-package ivy
  :delight
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1))

(use-package ivy-rich
  :config
  (ivy-rich-mode +1))

(use-package lsp-haskell
  :demand
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

(use-package multi-line
  :bind
  ("C-c l m" . multi-line)
  ("C-c l s" . multi-line-single-line))

(use-package ns-win
  :config
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'super)))

(use-package proced
  :hook
  (proced-mode . (lambda () (proced-toggle-auto-update +1)))
  :config
  (setq proced-auto-update-interval 1))

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (if (file-directory-p "~/src")
      (setq projectile-project-search-path '("~/src")))
  (projectile-discover-projects-in-search-path))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :delight
  :hook
  (prog-mode . rainbow-mode))

(use-package restclient
  :mode
  ("\\.restclient\\'" . restclient-mode))

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

(use-package scroll-bar
  :config
  ;; Disable stubborn scroll bars in emacsclient
  ;; https://emacs.stackexchange.com/a/46632
  (customize-set-variable 'scroll-bar-mode nil)
  (customize-set-variable 'horizontal-scroll-bar-mode nil))

(use-package shell-pop
  :bind
  ("C-c t" . shell-pop))

(use-package simple
  :config
  (column-number-mode +1))

(use-package smartparens
  :delight
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)
  (smartparens-global-mode +1)
  (show-smartparens-global-mode +1))

(use-package string-inflection
  :bind
  ("C-c q !" . string-inflection-upcase)
  ("C-c q C" . string-inflection-camelcase)
  ("C-c q S" . string-inflection-capital-underscore)
  ("C-c q c" . string-inflection-lower-camelcase)
  ("C-c q k" . string-inflection-kebab-case)
  ("C-c q q" . string-inflection-all-cycle)
  ("C-c q s" . string-inflection-underscore))

(use-package subword
  :delight)

(use-package swiper
  :bind
  ("C-c C-r" . ivy-resume)
  ("C-s" . swiper-isearch))

(use-package systemd
  :mode ("\\.service"))

(use-package tooltip
  :config
  (tooltip-mode -1)
  (setq tooltip-use-echo-area t))

(use-package unfill
  :bind
  ("M-q" . unfill-toggle))

(use-package which-key
  :delight
  :config
  (which-key-mode)
  (which-key-declare-prefixes
    "C-c f" "files"
    "C-c g" "git"
    "C-c l" "multi-line"
    "C-c p" "projectile"
    "C-c q" "string-inflection"
    "C-c T" "theme"))

(use-package ws-butler
  :delight
  :hook
  (prog-mode . ws-butler-mode))

(provide 'init)

;;; init.el ends here
