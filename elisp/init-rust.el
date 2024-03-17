;;; Package --- init-rust.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Rust Development

;;; Code:

(use-package rust-mode
  :straight t
  :hook
  (rust-mode . eglot-ensure)
  :config
  (setq rust-format-on-save t)
  (add-to-list 'eglot-server-programs
             '((rust-ts-mode rust-mode) .
               ("rust-analyzer" :initializationOptions
                (:check (:command "clippy")))))
  :init
  (setq rust-mode-treesitter-derive t)
  :ensure-system-package
  ((rustup . "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh")
   (rust-analyzer . "rustup component add rust-analyzer"))
  )

(use-package cargo-mode
  :straight t
  :hook
  (rust-mode . cargo-minor-mode)
  :config
  (setq compilation-scroll-output t))

(provide 'init-rust)
;;; init-rust.el ends here
