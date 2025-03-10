;; Package --- init-lsp.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Language Server Protocol Settings

;;; Code:

;; Function to get TypeScript SDK path for Volar
(defun vue-eglot-init-options ()
  (let ((tsdk-path (expand-file-name "lib"
                   (string-trim-right
                    (shell-command-to-string "npm list --global --parseable typescript | head -n1")))))
    `(:typescript (:tsdk ,tsdk-path
                  :languageFeatures (:completion (:defaultTagNameCase "both"
                                                :defaultAttrNameCase "kebabCase"
                                                :getDocumentNameCasesRequest nil
                                                :getDocumentSelectionRequest nil)
                                   :diagnostics (:getDocumentVersionRequest nil))
                  :documentFeatures (:documentFormatting (:defaultPrintWidth 100
                                                        :getDocumentPrintWidthRequest nil)
                                   :documentSymbol t
                                   :documentColor t)))))

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
  (python-ts-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  (js-ts-mode . eglot-ensure)
  (vue-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               `(vue-mode . ("vue-language-server" "--stdio"
                             :initializationOptions ,(vue-eglot-init-options))))
  :init
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (local-leader-def
    :keymaps 'eglot-mode-map
    "l"  '(:ignore t :which-key "lsp")
    "lr" '(eglot-rename :which-key "Rename")
    "la" '(eglot-code-actions :which-key "Code Actions")
    )
  (leader-def
    :keymaps 'eglot-mode-map
    "l"  '(:ignore t :which-key "lsp")
    "lr" '(eglot-reconnect :which-key "Reconnect")))

(provide 'init-lsp)
;;; init-lsp.el ends here
