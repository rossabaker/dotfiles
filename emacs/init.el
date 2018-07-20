(require 'package)
;; optional. makes unpure packages archives unavailable
(setq package-archives nil)
(setq package-enable-at-startup nil)
(package-initialize)

(eval-when-compile
  (require 'use-package))

(use-package better-defaults)

(use-package ace-window
  :bind ("C-c o" . ace-window))

(use-package aggressive-indent
  :diminish
  :hook (prog-mode . aggressive-indent-mode))

(use-package autorevert
  :diminish auto-revert-mode
  :custom
  (auto-revert-verbose nil)
  :config
  (global-auto-revert-mode))

(use-package avy
  :bind (("C-'" . avy-goto-char-timer)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1))
  :config
  (avy-setup-default))

(use-package avy-zap
  :custom
  (avy-zap-dwim-prefer-avy nil)
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

(use-package beacon
  :diminish
  :bind ("C-c C-b" . beacon-blink)
  :config
  (beacon-mode t))

(use-package change-inner
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))

(use-package company
  :diminish
  :custom
  (company-idle-delay nil)
  :bind ("TAB" . company-indent-or-complete-common)
  :config
  (global-company-mode t))

(use-package copy-as-format
  :custom
  (copy-as-format-default "github")
  :bind ("C-c M-w" . copy-as-format))

(use-package counsel
  :after ivy
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h l" . counsel-find-library)
         ("C-h i" . counsel-info-lookup-symbol)
         ("C-h u" . counsel-unicode-char)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-c l" . counsel-locate)
         ("C-c q" . counsel-set-variable)))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package delsel
  :config
  (delete-selection-mode t))

(use-package desktop
  :config
  (desktop-save-mode t))

(use-package diff-hl
  :hook (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode t)
  (diff-hl-flydiff-mode t))

(use-package dired
  :custom
  (dired-auto-revert-buffer t))

(use-package display-line-numbers
  :hook ((prog-mode text-mode) . display-line-numbers-mode))

(use-package dumb-jump
  :custom (dumb-jump-selector 'ivy)
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window)))

(use-package edit-server
  :config
  (edit-server-start))

(use-package eldoc
  :diminish)

(use-package free-keys)

(use-package flycheck
  :diminish
  :config
  (global-flycheck-mode t))

(use-package flyspell
  :diminish
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package guru-mode
  :diminish
  :config
  (guru-global-mode t))

(use-package haskell-mode
  :config
  (setq haskell-process-args-ghci '("-ferror-spans" "-fshow-loaded-modules"))
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(use-package ialign
  :bind ("C-x l" . ialign))

(use-package ivy-mode
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode t)
  :bind (("C-c C-r" . ivy-resume)
         ("C-x b" . ivy-switch-buffer)))

(use-package know-your-http-well
  :commands (http-header
             http-method
             http-relation
             http-status-code
             media-type))

(use-package magit
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  :bind ("C-c g" . magit-status))

(use-package material-theme
  :ensure t
  :config
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (load-theme 'material t)))))

(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode)))

(use-package multiple-cursors
  :bind (("C-c m ^"     . mc/edit-beginnings-of-lines)
         ("C-c m `"     . mc/edit-beginnings-of-lines)
         ("C-c m $"     . mc/edit-ends-of-lines)
         ("C-c m '"     . mc/edit-ends-of-lines)
         ("C-c m R"     . mc/reverse-regions)
         ("C-c m S"     . mc/sort-regions)
         ("C-c m W"     . mc/mark-all-words-like-this)
         ("C-c m Y"     . mc/mark-all-symbols-like-this)
         ("C-c m a"     . mc/mark-all-like-this-dwim)
         ("C-c m c"     . mc/mark-all-dwim)
         ("C-c m l"     . mc/insert-letters)
         ("C-c m n"     . mc/insert-numbers)
         ("C-c m r"     . mc/mark-all-in-region)
         ("C-c m s"     . set-rectangular-region-anchor)
         ("C-c m %"     . mc/mark-all-in-region-regexp)
         ("C-c m t"     . mc/mark-sgml-tag-pair)
         ("C-c m w"     . mc/mark-next-like-this-word)
         ("C-c m x"     . mc/mark-more-like-this-extended)
         ("C-c m y"     . mc/mark-next-like-this-symbol)
         ("C-c m C-SPC" . mc/mark-pop)
         ("C-c m ("     . mc/mark-all-symbols-like-this-in-defun)
         ("C-c m C-("   . mc/mark-all-words-like-this-in-defun)
         ("C-c m M-("   . mc/mark-all-like-this-in-defun)
         ("C-c m ["     . mc/vertical-align-with-space)
         ("C-c m {"     . mc/vertical-align)
         ("S-<down-mouse-1>")
         ("S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

(use-package projectile
  :config
  (projectile-global-mode t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

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
