;;; init-elisp.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Enhance emacs-lisp-mode

(use-package elisp-slime-nav
  :straight t
  :ensure t
  :defer t
  :hook
  (emacs-lisp-mode . turn-on-elisp-slime-nav-mode)
  (emacs-lisp-mode . turn-on-elisp-slime-nav-mode)
  :config
  (general-def 'normal emacs-lisp-mode-map
    "K" 'elisp-slime-nav-describe-elisp-thing-at-point)
  )

(provide 'init-elisp)
;;; init-elisp.el ends here
