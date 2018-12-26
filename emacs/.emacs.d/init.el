;;; init.el --- Ross A. Baker's Emacs configuration

;;; Commentary:
;;
;; This is my Emacs configuration, and this commentary appeases flycheck.

;;; Code:

;; Configure package manager
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Configure general first, because we use it in other use-packages

(use-package general
 :ensure t
 :config
 (general-create-definer ross/leader-def
   :prefix "C-c"))

;; Packages

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-material t nil))

(use-package browse-url
  :config
  (setq browse-url-chrome-program "google-chrome-beta"
	browse-url-browser-function 'browse-url-chrome))

(use-package counsel
  :ensure t
  :config
  :general
  ("M-x" 'counsel-M-x)
  ("C-x C-f" 'counsel-find-file)
  ("C-h f" 'counsel-describe-function)
  ("C-h v" 'counsel-describe-variable)
  ("C-h l" 'counsel-find-library)
  ("C-h i" 'counsel-info-lookup-symbol)
  ("C-x 8 RET" 'counsel-unicode-char)
  (ross/leader-def
    "/"  'counsel-git-grep
    "sp" 'counsel-git-grep
    "fg" 'counsel-git
    "fl" 'counsel-locate))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode 1)
  :general
  (ross/leader-def
    "p" '(:keymap projectile-command-map)))

(use-package csv-mode
  :ensure t)

(use-package cus-edit
  :config
  ;; Banish customizations to a gitignored file.
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (load custom-file 'noerror))

(use-package easy-kill
  :ensure t
  :general
  ("M-w" 'easy-kill)
  ("C-M-@" 'easy-mark)
  ("C-M-SPC" 'easy-mark))

(use-package ediff-wind
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package engine-mode
  :ensure t
  :config
  (defengine amazon
    "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s"
    :keybinding "a")
  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "h")
  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")
  (defengine google-images
    "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s"
    :keybinding "i")
  (defengine google-maps
    "http://maps.google.com/maps?q=%s"
    :keybinding "m")
  (defengine rfcs
    "http://pretty-rfc.herokuapp.com/search?q=%s"
    :keybinding "r")
  (defengine twitter
    "https://twitter.com/search?q=%s"
    :keybinding "t")
  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w")
  (defengine wiktionary
    "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s"
    :keybinding "d")
  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :keybinding "y"))

(use-package files
  :config
  (defun ross/reload-user-init-file ()
    (interactive)
    (load-file user-init-file))
  (defun ross/find-user-init-file ()
    (interactive)
    (find-file user-init-file))
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
	require-final-newline t)
  :general
  (ross/leader-def
    "fei" 'ross/find-user-init-file
    "fer" 'ross/reload-user-init-file))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode 1)
  :general
  (ross/leader-def
   "ec" 'flycheck-clear
   "eh" 'flycheck-describe-checker
   "el" 'flycheck-list-errors
   "ee" 'flycheck-explain-error-at-point
   "es" 'flycheck-select-checker
   "eS" 'flycheck-set-checker-executable
   "ev" 'flycheck-verify-setup))

(use-package flyspell
  :ensure t
  :config
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(use-package flyspell-correct-ivy
  :ensure t
  :config
  (setq flyspell-correct-interface #'flyspell-correct-ivy)
  :general
  (:keymaps 'flyspell-mode-map
            "C-;" 'flyspell-correct-wrapper))

(use-package frame
  :config
  (set-face-attribute 'default nil
		      :family "Hasklig"
		      :height 120
		      :weight 'normal))

(use-package git-link
  :ensure t
  :config
  (progn
    ;; This is mostly all a hack around my gh alias.
    (defun ross//git-link-gh (hostname dirname filename branch commit start end)
      (git-link-github "github.com" dirname filename branch commit start end))
    (defun ross//git-link-commit-gh (hostname dirname commit)
      (git-link-commit-github "github.com" dirname commit))
    (defun ross/git-link-homepage (remote)
      (interactive (list (git-link--select-remote)))
      (let* ((remote-url (git-link--remote-url remote))
             (remote-info (when remote-url (git-link--parse-remote remote-url))))
        (message (car remote-info))
        (if (eq (car remote-info) "gh")
            (git-link--new (format "https://github.com/%s" (cadr remote-info)))
          (git-link-homepage remote))))
    (add-to-list 'git-link-remote-alist
                 '("gh" ross//git-link-gh))
    (add-to-list 'git-link-commit-remote-alist
                 '("gh" ross//git-link-commit-gh))

    (defun ross/git-link-browse ()
      (interactive)
      (let ((git-link-open-in-browser t))
        (call-interactively 'git-link)))
    (defun ross/git-link-commit-browse ()
      (interactive)
      (let ((git-link-open-in-browser t))
        (call-interactively 'git-link-commit)))
    (defun ross/git-link-homepage-browse ()
      (interactive)
      (let ((git-link-open-in-browser t))
        (call-interactively 'ross/git-link-homepage)))
  :general
  (ross/leader-def
    "gll" 'ross/git-link-browse
    "glL" 'git-link
    "glc" 'ross/git-link-commit-browse
    "glC" 'git-link-commit
    "glh" 'ross/git-link-homepage-browse
    "glH" 'ross/git-link-homepage)))

(use-package git-messenger
  :ensure t
  :general
  :general
  (ross/leader-def
    "gM" 'git-messenger:popup-message))

(use-package git-timemachine
  :ensure t
  :general
  (ross/leader-def
    "gt" 'git-timemachine))

(use-package hasklig-mode
  :ensure t
  :hook (haskell-mode))

(use-package hippie-exp
  :general
  ("M-/" 'hippie-expand))

(use-package ibuffer
  :general
  ("C-x C-b" 'ibuffer))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  :general
  (ross/leader-def
    "rl" 'ivy-resume))

(use-package lsp-mode
  :ensure t)

(use-package lsp-scala
  :load-path "~/src/lsp-scala"
  :after scala-mode
  :demand t
  :config
  (setq lsp-scala-server-command "~/bin/metals-emacs")
  :hook 'scala-mode-hook #'lsp)

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode))

(use-package magit
  :ensure t
  :general
  (ross/leader-def 
    "gb" 'magit-blame
    "gc" 'magit-clone
    "gm" 'magit-dispatch-popup
    "gs" 'magit-status
    "gS" 'magit-stage-file
    "gU" 'magit-unstage-file))
  
(use-package menu-bar
  :config
  (menu-bar-mode -1))

(use-package scroll-bar
  :config
  (scroll-bar-mode -1))

(use-package paren
  :config
  (setq show-paren-delay 0)
  (show-paren-mode 1))

(use-package projectile
  :ensure t
  :config
  (projectile-mode 1))

(use-package protobuf-mode
  :ensure t)

(use-package saveplace
  :config
  (save-place-mode 1))

(use-package sbt-mode
  :ensure t
  :commands sbt-start sbt-command
  :after scala-mode
  :config
  (ross//scala-bindings 'sbt:mode-map))

(use-package scala-mode
  :ensure t
  :config
  (defun ross//scala-bindings (mode-map)
    (ross/leader-def 
      :keymaps mode-map
      "mb." 'sbt-hydra
      "mbb" 'sbt-command
      "mbc" 'ross/sbt-test-compile
      "mbC" 'ross/sbt-clean
      "mbi" 'sbt-start
      "mbp" 'ross/sbt-package
      "mbr" 'ross/sbt-run
      "mbt" 'ross/sbt-test))
  (defun ross/sbt-test-compile ()
    (interactive)
    (sbt-command "test:compile"))
  (defun ross/sbt-clean ()
    (interactive)
    (sbt-command "clean"))
  (defun ross/sbt-clean-test-compile ()
    (interactive)
    (sbt-command ";clean ;test:compile"))
  (defun ross/sbt-package ()
    (interactive)
    (sbt-command "package"))
  (defun ross/sbt-run ()
    (interactive)
    (sbt-command "run"))
  (defun ross/sbt-test ()
    (interactive)
    (sbt-command "test"))
  (ross//scala-bindings 'scala-mode-map))

(use-package simple
  ;; Includes some defaults defined in C source code
  :config
  (setq indent-tabs-mode nil
	load-prefer-newer t
	save-interprogram-paste-before-kill t))

(use-package swiper
  :config
  :general
  ("C-s" 'swiper))

(use-package tool-bar
  :config
  (tool-bar-mode -1))

(use-package tooltip
  :config
  (tooltip-mode -1))

(use-package try
  :ensure t)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(provide 'init)
;;; init.el ends here
