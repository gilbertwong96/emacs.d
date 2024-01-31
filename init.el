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
  (straight-check-for-modifications '(watch-files find-when-checking))
  (straight-use-package-by-default nil)
  )

;; Place user settings to elisp dir
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

;; Patch for emacs lisp
(use-package el-patch :straight t :defer t)

(use-package protobuf-mode :straight t :defer t :ensure t)

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
(use-package init-just)
(use-package init-lsp)
(use-package init-dockerfile)

(use-package init-config)
(use-package init-elisp)
(use-package init-shell)
(use-package init-erlang)
(use-package init-haskell)
(use-package init-golang)
(use-package init-markdown)
(use-package init-rust)
(use-package init-elixir)


;; ;; Knowledge Management System
(use-package init-org)

(provide 'init)
;;; init.el ends here
