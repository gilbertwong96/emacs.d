;;; Package --- init-lsp.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Language Server Protocol Settings

;;; Code:

(use-package eglot
  :straight t
  :ensure t
  :custom
  (eglot-send-changes-idle-time 1 "Send changes after 1 second idle time")
  :defer t
  :hook
  (eglot-managed-mode . flymake-mode)
  (c-mode . eglot-ensure)
  (erlang-mode . eglot-ensure)
  (sh-mode . eglot-ensure)
  (go-mode . eglot-ensure)
  (haskell-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '(erlang-mode . ("elp" "server")))
  :init
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (local-leader-def
    :keymaps 'eglot-mode-map
    "lr" 'eglot-rename
    )
  (leader-def
    :keymaps 'eglot-mode-map
    "lr" 'eglot-reconnect
    )
  )

(provide 'init-lsp)
;;; init-lsp.el ends here
