;;; Package --- init-haskell.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Gilbert's Emacs Configuration

;;; Code:

(use-package haskell-mode
  :straight t
  :ensure t
  :defer t
  :ensure-system-package haskell
  :hook
  (haskell-mode . eglot-ensure))

(provide 'init-haskell)
;;; init-haskell.el ends here
