;;; init-tree-sitter.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tree-Sitter Settings

;;; Code:

(use-package treesit-auto
  :straight t
  :ensure t
  :config
  (setq treesit-auto-install 'markdown)
  (global-treesit-auto-mode))

(provide 'init-treesitter)
;;; init-tree-sitter.el ends here
