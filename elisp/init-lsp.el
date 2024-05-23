;; Package --- init-lsp.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Language Server Protocol Settings

;;; Code:

(use-package eglot
  :straight t
  :ensure t
  ;; :custom
  ;; (eglot-send-changes-idle-time 0.5 "Send changes after 1 second idle time")
  :defer t
  :hook
  (c-mode . eglot-ensure)
  (erlang-mode . eglot-ensure)
  (sh-mode . eglot-ensure)
  (elixir-ts-mode . eglot-ensure)
  ;; :config
  ;; (add-to-list 'eglot-server-programs
  ;;              '(erlang-mode . ("elp" "server")))
  :init
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (local-leader-def
    :keymaps 'eglot-mode-map
    "l"  '(:ignore t :which-key "lsp")
    "lr" '(eglot-rename :which-key "Rename")
    )
  (leader-def
    :keymaps 'eglot-mode-map
    "l"  '(:ignore t :which-key "lsp")
    "lr" '(eglot-reconnect :which-key "Reconnect"))
  )

(provide 'init-lsp)
;;; init-lsp.el ends here
