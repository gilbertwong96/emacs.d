;;; init-rust.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Rust Development

;;; Code:

(use-package rustic
  :straight t
  :defer t
  :hook
  (eglot--managed-mode . (lambda () (flymake-mode -1)))
  :init
  (setq rustic-lsp-client 'eglot)
  )

(provide 'init-rust)
;;; init-rust.el ends here
