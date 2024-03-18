;;; Package --- init-haskell.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Gilbert's Emacs Configuration

;;; Code:

(use-package haskell-mode
  :straight t
  :ensure t
  :if (eq system-type 'darwin)
  :ensure-system-package (ghc haskell-language-server)
  :if (eq system-type 'gnu/linux)
  :ensure-system-package
  ((ghc . "brew install ghc")
   (haskell-language-server . "brew install haskell-language-server"))
  :hook
  (haskell-mode . eglot-ensure))

(provide 'init-haskell)
;;; init-haskell.el ends here
