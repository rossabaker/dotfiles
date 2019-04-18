;;; Bootstrap

;;;; straight.el

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;; use-package

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

;;;; Better defaults

;; I used to use https://github.com/technomancy/better-defaults, but
;; it fought with no-littering and foists ido upon me.  No wonder
;; changing defaults is so hard.
;;
;; But these are truly better.

;;; Turn off obnoxious default UI elements

(dolist (mode '(menu-bar-mode
		tool-bar-mode
		scroll-bar-mode
		horizontal-scroll-bar-mode
		blink-cursor-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;;; Packages, alphabetically

(use-package base16-theme
  :config
  (load-theme 'base16-materia t))

(use-package counsel
  :bind
  ("C-c /" . counsel-git-grep)
  ("C-c c" . counsel-compile)
  ("C-c f l" . counsel-locate)
  ("C-c f f" . counsel-find-file)
  :config
  (counsel-mode 1))

(use-package ivy
  :bind
  ("C-c C-r" . ivy-resume)
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1))

(use-package no-littering
  :demand t
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-var-file-name "custom.el")))

(use-package magit
  :bind (("C-c g s" . magit-status)))

(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-project-search-path '("~/src"))
  (projectile-discover-projects-in-search-path))

(use-package swiper
  :bind ("C-s" . swiper))

