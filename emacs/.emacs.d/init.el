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

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-material t nil))

(use-package counsel
  :ensure t
  :config
  ;; (global-set-key (kbd "M-x") 'counsel-M-x)
  ;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  ;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  ;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  ;; (global-set-key (kbd "<f1> l") 'counsel-find-library)
  ;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  ;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  ;; (global-set-key (kbd "C-c g") 'counsel-git)
  ;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
  ;; (global-set-key (kbd "C-c k") 'counsel-ag)
  ;; (global-set-key (kbd "C-x l") 'counsel-locate)
  ;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  )

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

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  ;; (global-set-key (kbd "C-c C-r") 'ivy-resume)
  )

(use-package menu-bar
  :config
  (menu-bar-mode -1))

(use-package scroll-bar
  :config
  (scroll-bar-mode -1))

(use-package swiper
  :config
  ;; (global-set-key (kbd "C-s") 'swiper)
  )

(use-package tool-bar
  :config
  (tool-bar-mode -1))

(use-package tooltip
  :config
  (tooltip-mode -1))
