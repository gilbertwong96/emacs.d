
;;; init-elixir.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Gilbert's Emacs Configuration

;;; Code:

(use-package elixir-mode
  :straight t
  :ensure t
  :defer t
  :hook
  (elixir-mode . eglot-ensure)
  )

(provide 'init-elixir)
;;; init-elixir.el ends here
