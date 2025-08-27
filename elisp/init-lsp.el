;; Package --- init-lsp.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Language Server Protocol Settings

;;; Code:

(use-package eglot
  :straight t
  :custom
  ;; (eglot-send-changes-idle-time 0.5 "Send changes after 1 second idle time")
  (eglot-sync-connect nil)
  :defer t
  :hook
  (c-mode . eglot-ensure)
  (erlang-mode . eglot-ensure)
  (sh-mode . eglot-ensure)
  (elixir-ts-mode . eglot-ensure)
  (python-ts-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  (js-ts-mode . eglot-ensure)
  (vue-ts-mode . eglot-ensure)
  (dart-mode . eglot-ensure)
  (swift-mode . eglot-ensure)
  :config
  (dolist (server `((vue-ts-mode . ("vue-language-server" "--stdio"))
                    (sql-mode . ("sqls"))
                    (swift-mode . ("sourcekit-lsp"))))
    (add-to-list 'eglot-server-programs server))
  :init
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (local-leader-def
    :keymaps 'eglot-mode-map
    "l"  '(:ignore t :which-key "lsp")
    "lr" '(eglot-rename :which-key "Rename")
    "la" '(eglot-code-actions :which-key "Code Actions"))
  (leader-def
    :keymaps 'eglot-mode-map
    "l"  '(:ignore t :which-key "lsp")
    "lr" '(eglot-reconnect :which-key "Reconnect")))


(use-package eglot-booster
  :straight (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

(provide 'init-lsp)
;;; init-lsp.el ends here
