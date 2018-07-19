(require 'package)
;; optional. makes unpure packages archives unavailable
(setq package-archives nil)
(setq package-enable-at-startup nil)
(package-initialize)

(eval-when-compile
  (require 'use-package))

(use-package better-defaults)

(use-package desktop
  :config
  (desktop-save-mode t))

(use-package haskell-mode
  :config
  (setq haskell-process-args-ghci '("-ferror-spans" "-fshow-loaded-modules"))
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(use-package magit
  :bind ("C-c m" . magit-status))

(use-package material-theme
  :ensure t
  :config
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (load-theme 'material t)))))

(use-package nix-mode)

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
