;;; used-packages --- Generates a Nix list of packages used in our config

;;; Commentary:

;; We run this on our init.el file to introspect the extra packages to
;; add to our Emacs Nix derivation.

(require 'use-package)

;;; Code:

(defvar ross/use-package-dependencies
  '(use-package delight bind-key)
  "Nix packages needed by use-package itself.")

(defvar ross/builtin-nix-emacs-packages
  '(ansi-color
    auth-source
    display-line-numbers-mode
    emacs
    flyspell
    hl-line
    imenu
    recentf
    savehist)
  "Packages we use that are built into Emacs.")

(defun ross/used-packages (file)
  "Output as a Nix list all packages in use-package declarations in FILE."
  (insert-file-contents file)
  (princ "epkgs: with epkgs; [ ")
  (dolist (dep ross/use-package-dependencies)
    (princ dep)
    (princ " "))
  (goto-char (point-min))
  (let ((re (eval use-package-form-regexp-eval)))
    (while (re-search-forward re nil t)
      (goto-char (match-beginning 0))
      (let* ((decl (read (current-buffer)))
             (name (cadr decl)))
        (when (and (eq (car decl) 'use-package)
                   (not (memq name ross/builtin-nix-emacs-packages))
                   (symbolp name))
          (princ (symbol-name name))
          (princ " ")))))
  (princ "] "))

(provide 'used-packages)
;;; used-packages ends here
