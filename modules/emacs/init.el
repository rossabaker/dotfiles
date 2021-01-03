;;; init.el --- Ross A. Baker's Emacs Configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Ross A. Baker

;; Author: Ross A. Baker <ross@rossabaker.com>
;; Keywords:

;;; Commentary:

;; Packages in this Emacs configuration are derived from
;; Nix.  `used-packages.el' introspects this file and extracts
;; Nix packages from the `use-package' statements.
;;
;; Most dependencies are on MELPA.  Those that aren't appear in the
;; overlay in default.nix.
;;
;; If you like what you see, copy, as I have done from others.  If you
;; see room for improvement, pull requests are welcome.  This is my
;; personal config, and I might not merge it, but I always enjoy a
;; good editor discussion.

;;; Code:

;;;; Bootstrap

(eval-when-compile
  ;; The default "-hook" impairs ripgreppability and help contexts
  (setq use-package-hook-name-suffix nil)
  (require 'use-package))

;;;; Personal

(setq user-full-name "Ross A. Baker"
      user-mail-address "ross@rossabaker.com")

;;;; Better Defaults

(use-package better-defaults
  :ensure
  :config
  (ido-mode nil) ;; Never ido
  )

(use-package no-littering
  ;; Don't shit where you eat.
  :ensure
  :custom
  (auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;;;; Completion

(use-package selectrum
  :ensure
  :config
  (selectrum-mode))

(use-package selectrum-prescient
  :ensure
  :after selectrum
  :config
  (selectrum-prescient-mode))

;;;; Projects

(use-package project
  ;; There have been advances since the one in Emacs 27.1.
  :ensure t)

;;;; Version control

(use-package magit
  :ensure
  :bind ("C-c g s" . magit-status))

;;;; Languages

;;;;; Scala

(use-package scala-mode
  :ensure
  :interpreter ("scala" . scala-mode))

(use-package sbt-mode
  :ensure
  :commands sbt-start sbt-command)

;;;; Packages

(use-package try
  :ensure
  ;; A downside of a Nix-managed Emacs is that new packages require a
  ;; restart. The reproducibility is generally worth it, but sometimes
  ;; we just want to fix an unfamiliar language or try something we
  ;; read on Emacs News.
  :config
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (defun ross/package-refresh-contents-maybe ()
    "Refresh the packages if we have no `package-archive-contents'."
    (unless package-archive-contents
      (package-refresh-contents)))
  (advice-add 'try :before 'ross/package-refresh-contents-maybe))

(provide 'init)

;;; init.el ends here
