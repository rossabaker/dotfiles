(eval-when-compile
  (require 'use-package))

(use-package base16-theme
  :config
  (load-theme 'base16-materia t))

(use-package better-defaults
  :config
  ;; better-defaults sets one worse default
  (ido-mode -1))

(use-package counsel
  :bind
  ("C-c /" . counsel-git-grep)
  ("C-c c" . counsel-compile)
  ("C-c f l" . counsel-locate)
  ("C-h f" . counsel-describe-function)
  ("C-h v" . counsel-describe-variable)
  ("C-h l" . counsel-find-library)
  ("C-h i" . counsel-info-lookup-symbol)
  ("C-x 8 RET" . counsel-unicode-char)
  ("C-x C-f" . counsel-find-file)
  ("M-x" . counsel-M-x))

(use-package ivy
  :delight
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1))

(use-package magit
  :bind
  ("C-c g s" . magit-status))

(use-package swiper
  :bind
  ("C-c C-r" . ivy-resume)
  ("C-s" . swiper-isearch))

(use-package ws-butler
  :delight
  :hook
  (prog-mode . ws-butler-mode))
