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
  (heex-ts-mode . eglot-ensure)
  (python-ts-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  (js-ts-mode . eglot-ensure)
  (dart-mode . eglot-ensure)
  (swift-mode . eglot-ensure)
  :config
  (dolist (server `((sql-mode . ("sqls"))
                    (swift-mode . ("sourcekit-lsp"))
                    ((tsx-ts-mode typescript-ts-mode)
                     . ("rass"
                        "--"
                        "typescript-language-server" "--stdio"
                        "--"
                        "oxlint" "--lsp"
                        "--"
                        "tailwindcss-language-server" "--stdio"))
                    ))
    (add-to-list 'eglot-server-programs server))
  ;; Register Expert for Elixir modes + install the :initialized interceptor
  (add-to-list 'eglot-server-programs
               '((elixir-ts-mode heex-ts-mode) . ("expert" "--stdio")))
  (advice-add 'jsonrpc-notify :before #'my/expert--send-config)

  (setq-default eglot-workspace-configuration
                '((:typescript
                   (:format
                    (:indentSize 2
                                 :tabSize 2
                                 :convertTabsToSpaces t))
                   :javascript
                   (:format
                    (:indentSize 2
                                 :tabSize 2
                                 :convertTabsToSpaces t)))))
  :init
  ;; -- Expert Elixir LSP helpers (defuns must be defined before use) -------
  (defun my/expert--resolve (cmd)
    (or (when-let* ((mise (executable-find "mise"))
                    (path (ignore-errors
                            (string-trim
                             (shell-command-to-string
                              (format "%s which %s" (shell-quote-argument mise) cmd)))))
                    ((not (string-empty-p path)))
                    ((file-executable-p path)))
          path)
        (executable-find cmd)))

  (defun my/expert--send-config (server method _params)
    (when (eq method :initialized)
      (when-let* ((elixir (my/expert--resolve "elixir"))
                  (erl    (my/expert--resolve "erl")))
        (jsonrpc-notify server :workspace/didChangeConfiguration
                        (list :settings
                              (list :elixirExecutablePath elixir
                                    :erlangExecutablePath erl))))))
  ;; ------------------------------------------------------------------------
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


(provide 'init-lsp)
;;; init-lsp.el ends here
