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

(use-package avy
  :bind
  ("C-'" . avy-goto-char-timer)
  :config
  (setq avy-style 'at-full)
  (avy-setup-default))

(use-package base16-theme
  :config
  (load-theme 'base16-materia t)
  (set-face-attribute 'line-number-current-line nil
                      :background (plist-get base16-materia-colors :base01)
                      :foreground (plist-get base16-materia-colors :base04)
                      :inverse-video nil))

(use-package better-defaults
  :config
  ;; better-defaults sets one worse default
  (ido-mode -1))

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
  ("C-c e" . crux-eval-and-replace)
  ("C-c r" . crux-rename-file-and-buffer)
  ("C-x 4 t" . crux-transpose-windows)
  ("C-S-<backspace>" . crux-kill-whole-line))

(use-package desktop
  :config
  (desktop-save-mode t))

(use-package display-line-numbers
  :hook
  (prog-mode . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-width 4))

(use-package eldoc
  :delight)

(use-package flycheck
  :delight
  :config
  (global-flycheck-mode))

(use-package frame
  :config
  (blink-cursor-mode -1)
  (setq default-frame-alist '((font . "Hasklig-14"))))

(use-package git-gutter
  :delight
  :config
  (global-git-gutter-mode +1)
  (custom-set-variables '(git-gutter:update-interval 1)))

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

(use-package lsp-mode
  :hook
  (scala-mode . lsp)
  :config
  (setq lsp-enable-snippet nil
        lsp-prefer-flymake nil))

(use-package lsp-ui
  :config
  (setq lsp-ui-sideline-show-hover nil))

(use-package magit
  :bind
  ("C-c g s" . magit-status))

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-project-search-path '("~/src"))
  (projectile-discover-projects-in-search-path))

(use-package sbt-mode
  :commands
  sbt-start
  sbt-command)

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

(use-package swiper
  :bind
  ("C-c C-r" . ivy-resume)
  ("C-s" . swiper-isearch))

(use-package systemd
  :mode ("\\.service"))

(use-package which-key
  :delight
  :config
  (which-key-mode))

(use-package ws-butler
  :delight
  :hook
  (prog-mode . ws-butler-mode))

(provide 'init)

;;; init.el ends here
