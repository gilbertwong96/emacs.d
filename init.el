;;; package --- Summary init.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Gilbert's Emacs Configuration

;;; Code:

(use-package benchmark-init
  :straight t
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package straight
  :custom
  (straight-use-package-by-default nil)
  (straight-check-for-modifications '(watch-files find-when-checking)))

;; (use-package use-package :straight t)

(use-package use-package-ensure-system-package
  :straight t
  :ensure t)

;; Place user settings to elisp dir
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

;; Patch for emacs lisp
(use-package el-patch :defer t)

;; Basic setting for editor
(use-package init-theme)
(use-package init-settings)
(use-package init-keybindings)
(use-package init-treemacs)
(use-package init-autocomplete)
(use-package init-vertico)
(use-package init-evil)
(use-package init-assistent)
(use-package init-terminal)

;; Develop Kits
(use-package init-git)
(use-package init-lsp)
(use-package init-docker)
(use-package init-config)
(use-package init-elisp)
(use-package init-shell)
(use-package init-erlang)
(use-package init-haskell)
(use-package init-golang)
(use-package init-markdown)
(use-package init-rust)
(use-package init-elixir)
(use-package init-treesit)

;; Knowledge Management System
(use-package init-org)

(use-package init-web)

;; Enhance editor
(use-package init-editor)

;; Reading
(use-package init-reading)

(provide 'init)
;;; init.el ends here
