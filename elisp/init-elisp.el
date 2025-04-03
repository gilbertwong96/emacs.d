;;; Package --- init-elisp.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Enhance emacs-lisp-mode

;;; Code:

(use-package paredit
  :straight t
  :hook
  (emacs-lisp-mode . paredit-mode))

(use-package package-lint :straight t)


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
  (general-def
    :states '(normal)
    :keymaps 'emacs-lisp-mode-map
    :keymap-filter (lambda (kmap)
                     (if (eq major-mode 'eask-mode)
                         nil ;; Disable this keymap in eask-mode
                       kmap))
    "K" 'elisp-slime-nav-describe-elisp-thing-at-point)
  )

(use-package flymake-elsa
  :straight (flymake-elsa :type git :host github :repo "flymake/flymake-elsa")
  :hook
  (emacs-lisp-mode . flymake-elsa-load))

(use-package eask
  :straight t
  :ensure t
  :hook
  (emacs-lisp-mode . eask-api-setup))

(use-package eldoc-eask
  :straight t
  :ensure t
  :after elisp-slime-nav
  :hook
  (eask-mode . (lambda () (eldoc-eask-enable)))
  (eask-mode . (lambda () (elisp-slime-nav-mode -1)))
  (eask-mode . (lambda () (flymake-mode -1)))
  :config
  (general-def
    :states '(normal)
    :keymaps 'eask-mode-map
    "K" 'eldoc))

(use-package easky
  :straight (easky :type git :host github :repo "emacs-eask/easky"))

(use-package eask-mode
  :straight t)

(customize-set-variable 'emacs-lisp-docstring-fill-column 100)

(provide 'init-elisp)
;;; init-elisp.el ends here
