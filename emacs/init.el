(eval-when-compile
  (require 'use-package))

(require 'delight)

(setq inhibit-startup-screen t
      initial-scratch-message "")

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
  (set-frame-font "Hasklig-14" nil t))

(use-package hasklig-mode
  :hook
  haskell-mode
  scala-mode)

(use-package ivy
  :delight
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1))

(use-package lsp-mode
  :hook
  (scala-mode . lsp)
  :config
  (setq lsp-enable-snippet nil
        lsp-prefer-flymake nil))

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

(use-package ws-butler
  :delight
  :hook
  (prog-mode . ws-butler-mode))
