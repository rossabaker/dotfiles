(eval-when-compile
  (require 'use-package))

(use-package base16-theme
  :config
  (load-theme 'base16-materia t))

(use-package better-defaults
  :config
  ;; better-defaults sets one worse default
  (ido-mode -1))

(use-package company-lsp)

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

(use-package delight)

(use-package flycheck
  :delight
  :config
  (global-flycheck-mode))

(use-package haskell-mode)

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

(use-package lsp-ui)

(use-package magit
  :bind
  ("C-c g s" . magit-status))

(use-package sbt-mode
  :commands
  sbt-start
  sbt-command)

(use-package scala-mode)

(use-package swiper
  :bind
  ("C-c C-r" . ivy-resume)
  ("C-s" . swiper-isearch))

(use-package ws-butler
  :delight
  :hook
  (prog-mode . ws-butler-mode))
