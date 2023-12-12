;;; init.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Gilbert's Emacs Configuration

;;; Code:


(use-package benchmark-init
  :straight t
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Patch for emacs lisp
(use-package el-patch :straight t :defer t)

(use-package protobuf-mode
  :straight t
  :defer t
  :ensure t)

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

(use-package init-cc)
(use-package init-config)
(use-package init-elisp)
(use-package init-shell)
(use-package init-erlang)
(use-package init-haskell)
(use-package init-golang)
(use-package init-markdown)
(use-package init-rust)
(use-package init-elixir)


;; Knowledge Management System
(use-package init-org)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-disable-insert-state-bindings t)
 '(evil-escape-delay 0.08)
 '(evil-escape-key-sequence [106 107])
 '(evil-escape-mode t)
 '(evil-mode t)
 '(evil-undo-system 'undo-redo)
 '(evil-want-keybinding nil)
 '(global-evil-collection-unimpaired-mode t)
 '(safe-local-variable-values '((allout-layout . t)))
 '(which-key-allow-evil-operators nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
