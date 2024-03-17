
;;; init-elixir.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Gilbert's Emacs Configuration

;;; Code:

;; (use-package elixir-mode
;;   :straight t
;;   :ensure t
;;   :defer t
;;   :hook
;;   (elixir-mode . eglot-ensure)
;;   )

(use-package elixir-ts-mode
  :straight t
  :ensure t
  :hook
  (elixir-ts-mode . eglot-ensure)
  :ensure-system-package elixir
  )

(provide 'init-elixir)
;;; init-elixir.el ends here
