(require 'package)
;; optional. makes unpure packages archives unavailable
(setq package-archives nil)
(setq package-enable-at-startup nil)
(package-initialize)

(eval-when-compile
  (require 'use-package))

(use-package better-defaults)

(use-package autorevert
  :custom
  (auto-revert-verbose nil)
  :config
  (global-auto-revert-mode))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h l" . counsel-find-library)
         ("C-h i" . counsel-info-lookup-symbol)
         ("C-h u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-c l" . counsel-locate)))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package delsel
  :config
  (delete-selection-mode t))

(use-package desktop
  :config
  (desktop-save-mode t))

(use-package dired
  :custom
  (dired-auto-revert-buffer t))

(use-package display-line-numbers
  :hook ((prog-mode text-mode) . display-line-numbers-mode))

(use-package guru-mode
  :config
  (guru-global-mode t))

(use-package haskell-mode
  :config
  (setq haskell-process-args-ghci '("-ferror-spans" "-fshow-loaded-modules"))
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(use-package ivy-mode
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode t)
  :bind ("C-c C-r" . ivy-resume))

(use-package magit
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  :bind ("C-c m" . magit-status))

(use-package material-theme
  :ensure t
  :config
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (load-theme 'material t)))))

(use-package nix-mode)

(use-package projectile
  :config
  (projectile-global-mode t))

(use-package recentf
  :custom
  (recentf-max-saved-items 100)
  :config
  (recentf-mode t))

(use-package savehist
  :config
  (savehist-mode t))

(use-package saveplace
  :config
  (save-place-mode t))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

(use-package swiper
  :bind ("C-s" . swiper))
