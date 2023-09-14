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

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :custom
  (copilot-idle-delay 1 "Set Copilot-idle-delay to 1s")
  :hook
  (prog-mode . copilot-mode)
  :config
  (define-key copilot-completion-map (kbd "C-j") 'copilot-accept-completion)
  (setq copilot-max-char 1000000)
  )


;; Patch for emacs lisp
(use-package el-patch :straight t :defer t)

(use-package init-settings)
(use-package init-evil)
(use-package init-keybindings)
(use-package init-treemacs)
(use-package init-autocomplete)
(use-package init-vertico)
(use-package init-just)
(use-package init-lsp)
(use-package init-config)
(use-package init-elisp)
(use-package init-shell)
(use-package init-erlang)
(use-package init-git)
(use-package init-markdown)
(use-package init-treesitter)
(use-package init-rust)

(use-package doom-modeline
  :straight t
  :ensure t
  :config
  (doom-modeline-mode 1)
  (setq inhibit-compacting-font-caches t))
