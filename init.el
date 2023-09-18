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

;; Init theme before the GUI initialized
(use-package init-theme)
(use-package init-settings)
(use-package init-keybindings)
(use-package init-treemacs)
(use-package init-autocomplete)
(use-package init-vertico)
(use-package init-just)
(use-package init-lsp)
(use-package init-evil)
(use-package init-assistent)
(use-package init-dockerfile)

(use-package init-config)
(use-package init-elisp)
(use-package init-shell)
(use-package init-erlang)
(use-package init-git)
(use-package init-markdown)
(use-package init-rust)
