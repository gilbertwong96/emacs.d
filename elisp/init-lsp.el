;;; init-lsp.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Language Server Protocol Settings

;;; Code:


(use-package eglot
  :straight t
  :ensure t
  :custom
  (eglot-send-changes-idle-time 1 "Send changes after 2 seconds idle time")
  :defer t
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
