;;; Package --- init-llm.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Integrate LLM Tools

;;; Code:

(use-package copilot
  :if (eq system-type 'darwin)
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :defer t
  ;; :bind (:map copilot-completion-map
  ;;             ("M-j" . 'copilot-accept-completion)
  ;;             )
              ;; ("TAB" . 'copilot-accept-completion)
              ;; ("C-TAB" . 'copilot-accept-completion-by-word)
              ;; ("C-<tab>" . 'copilot-accept-completion-by-word)))
  :custom
  (copilot-idle-delay 1 "Set Copilot-idle-delay to 1s")
  :hook
  (erlang-mode . copilot-mode)
  :config
  (setq copilot-max-char 1000000)
  (general-def 'insert copilot-mode-map
    "M-RET" 'copilot-accept-completion)
  )

(use-package gptel
  :straight t
  :defer t
  :custom
  (gptel-model 'deepseek-reasoner)
  (gptel-backend
   ;; VolcEngine DeepSeek offers an OpenAI compatible API
  (gptel-make-openai "DeepSeek"       ;Any name you want
    :host "ark.cn-beijing.volces.com/api/v3/chat/completions"
    ;; :host "api.deepseek.com"
    :endpoint "/chat/completions"
    :stream t
    :key (lambda () (string-trim
                (shell-command-to-string "op read op://Personal/deepseek-volce-api-key/password")))
    :models '(deepseek-reasoner deepseek-chat))
   ;; (gptel-make-ollama "Ollama"          ;Any name of your choosing
   ;;   :host "localhost:11434"            ;Where it's running
   ;;   :stream t                          ;Stream responses
   ;;   :models '(qwq:latest
   ;;             )))
   ))

(use-package smerge-mode
  :straight t
  :ensure nil
  :hook
  (prog-mode . smerge-mode))

(use-package elysium
  :straight t
  :after gptel
  :custom
  ;; Below are the default values
  (elysium-window-size 0.33) ; The elysium buffer will be 1/3 your screen
  (elysium-window-style 'vertical)) ; Can be customized to horizontal

(use-package magit-gptcommit
  :straight t
  :demand t
  :after magit
  :bind (:map git-commit-mode-map
              ("C-c C-g" . magit-gptcommit-commit-accept))
  :custom
  (magit-gptcommit-llm-provider
   (make-llm-ollama :embedding-model "mistral:latest"
                    :chat-model "mistral:latest"
                    :default-chat-temperature 0.2))
  ;; (magit-gptcommit-llm-provider (make-llm-openai :key "OPENAI-KEY"))

  :config
  ;; Enable magit-gptcommit-mode to watch staged changes and generate commit message automatically in magit status buffer
  ;; This mode is optional, you can also use `magit-gptcommit-generate' to generate commit message manually
  ;; `magit-gptcommit-generate' should only execute on magit status buffer currently
  ;; (magit-gptcommit-mode 1)

  ;; Add gptcommit transient commands to `magit-commit'
  ;; Eval (transient-remove-suffix 'magit-commit '(1 -1)) to remove gptcommit transient commands
  (magit-gptcommit-status-buffer-setup)
  :init
  (require 'llm-ollama))


(provide 'init-llm)
;;; init-llm.el ends here.
