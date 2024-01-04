;;; Package --- init-lsp.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Language Server Protocol Settings

;;; Code:

(use-package eglot
  :straight t
  :ensure t
  :custom
  (eglot-send-changes-idle-time 1 "Send changes after 2 seconds idle time")
  :defer t
  :hook
  (eglot-managed-mode . flymake-mode)
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

(use-package eglot-x
  :straight (:host github :repo "nemethf/eglot-x" :files ("*.el"))
  :ensure t
  :after eglot)

(provide 'init-lsp)
;;; init-lsp.el ends here
