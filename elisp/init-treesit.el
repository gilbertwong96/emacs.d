;;; Package --- init-treesit.el -*- lexical-binding: t; -*-

;;; Commentary:

;; TreeSit Settings

;;; Code:

(use-package treesit-auto
  :straight t
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :init
  (setq major-mode-remap-alist
      '((python-mode . python-ts-mode)))
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package treesit-fold
  :straight (treesit-fold
             :type git
             :host github
             :repo "emacs-tree-sitter/treesit-fold")
  :ensure t
  :config
  (global-treesit-fold-mode))

(provide 'init-treesit)
;;; init-treesit.el ends here
