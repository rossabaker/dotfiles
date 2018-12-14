;; Configure package manager
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Functions

(defun ross/reload-user-init-file ()
  (interactive)
  (load-file user-init-file))

;; Packages

(use-package cus-edit
  :config
  ;; Banish customizations to a gitignored file.
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (load custom-file 'noerror))

(use-package frame
  :config
  (set-face-attribute 'default nil
		      :family "Hasklig"
		      :height 120
		      :weight 'normal
		      :width 'normal))

(use-package hasklig-mode
  :ensure t
  :hook (haskell-mode))

(use-package menu-bar
  :config
  (menu-bar-mode -1))

(use-package scroll-bar
  :config
  (scroll-bar-mode -1))

(use-package tool-bar
  :config
  (tool-bar-mode -1))

(use-package tooltip
  :config
  (tooltip-mode -1))
