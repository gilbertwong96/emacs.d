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
  :defer t
  :custom
  (copilot-idle-delay 1 "Set Copilot-idle-delay to 1s")
  :hook
  (prog-mode . copilot-mode)
  :config
  (define-key copilot-completion-map (kbd "C-j") 'copilot-accept-completion)
  (setq copilot-max-char 1000000)
  )

(use-package dockerfile-mode
  :straight t
  :defer t
  )

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
;; (use-package init-assistent)

(use-package init-config)
(use-package init-elisp)
(use-package init-shell)
(use-package init-erlang)
(use-package init-git)
(use-package init-markdown)
(use-package init-rust)
