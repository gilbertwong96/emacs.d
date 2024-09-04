
;;; init-elixir.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Gilbert's Emacs Configuration

;;; Code:

;; (use-package elixir-mode
;;   :straight t
;;   :ensure t
;;   :defer t
;;   :hook
;;   (elixir-mode . eglot-ensure)
;;   )

(use-package elixir-ts-mode
  :straight t
  :ensure t
  :defer t
  :ensure-system-package elixir
  )

(use-package mix
  :straight t
  :ensure t
  :defer t
  :hook
  (elixir-ts-mode . mix-minor-mode)
  (heex-ts-mode . display-line-numbers-mode)
  :init
  (local-leader-def
    :keymaps 'mix-minor-mode-map
    "m"  '(:ignore t :which-key "mix")
    "mt" '(mix-execute-task :which-key "Execute Task")
    )
  )

(provide 'init-elixir)
;;; init-elixir.el ends here
