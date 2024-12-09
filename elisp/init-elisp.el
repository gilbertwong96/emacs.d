;;; Package --- init-elisp.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Enhance emacs-lisp-mode

;;; Code:

(use-package paredit
  :straight t
  :hook
  (emacs-lisp-mode . paredit-mode)
  )

(use-package elisp-slime-nav
  :straight t
  :ensure t
  :defer t
  :hook
  (emacs-lisp-mode . turn-on-elisp-slime-nav-mode)
  (emacs-lisp-mode . flymake-mode)
  (emacs-lisp-mode . (lambda () (setq truncate-lines t)))
  (emacs-lisp-mode . prettify-symbols-mode)
  :config
  (general-def 'normal emacs-lisp-mode-map
    "K" 'elisp-slime-nav-describe-elisp-thing-at-point)
  )

(customize-set-variable 'emacs-lisp-docstring-fill-column 100)

(provide 'init-elisp)
;;; init-elisp.el ends here
