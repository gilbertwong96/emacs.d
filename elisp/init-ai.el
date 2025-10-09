;;; Package --- init-ai.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Integrate AI Tools

;;; Code:

(defun get-openrouter-api-key ()
  "Get OpenRouter API Key from 1password."
  (string-trim (shell-command-to-string "op read op://AI/OpenRouter-Gemini/credential")))

(defun get-gemini-api-key ()
  "Get OpenRouter API Key from 1password."
  (string-trim (shell-command-to-string "op read 'op://AI/Google AI Studio API/credential'")))

(defun get-deepseek-api-key ()
  "Get DeepSeek API Key from 1password."
  (string-trim (shell-command-to-string "op read op://AI/deepseek-api-key/password")))

(defun get-openai-api-key ()
  "Get OpenAI API Key from 1password."
  (string-trim (shell-command-to-string "op read op://AI/OpenAI/credential")))

(defun get-context7-api-key ()
  "Get context 7 API Key from 1password."
  (string-trim (shell-command-to-string "op read op://AI/Context7/credential")))

(defun get-brave-api-key ()
  "Get context 7 API Key from 1password."
  (string-trim (shell-command-to-string "op read op://AI/BraveAPI/credential")))

(use-package gptel
  :straight t
  :custom
  (gptel-tools)
  :config
  (require 'gptel-integrations)
  (gptel-make-tool
   :name "read_buffer"              ; javascript-style snake_case name
   :function (lambda (buffer)            ; the function that will run
               (unless (buffer-live-p (get-buffer buffer))
                 (error "error: Buffer %s is not live" buffer))
               (with-current-buffer  buffer
                 (buffer-substring-no-properties (point-min) (point-max))))
   :description "return the contents of an emacs buffer"
   :args (list '(:name "buffer"
                       :type string     ; :type value must be a symbol
                       :description "the name of the buffer whose contents are to be retrieved"))
   :category "emacs")                ; An arbitrary label for grouping
  (gptel-make-tool
   :name "create_file"             ; javascript-style  snake_case name
   :function (lambda (path filename content) ; the function that runs
               (let ((full-path (expand-file-name filename path)))
                 (with-temp-buffer
                   (insert content)
                   (write-file full-path))
                 (format "Created file %s in %s" filename path)))
   :description "Create a new file with the specified content"
   :args (list '(:name "path"      ; a list of argument specifications
                       :type string
                       :description "The directory where to create the file")
               '(:name "filename"
                       :type string
                       :description "The name of the file to create")
               '(:name "content"
                       :type string
                       :description "The content to write to the file"))
   :category "filesystem")           ; An arbitrary label for grouping
  )

(use-package mcp
  :straight t
  :after gptel
  :custom (mcp-hub-servers
           `(("context7" . (:command "npx"
                                     :args ("-y" "@upstash/context7-mcp" "--api-key"
                                            ,(get-context7-api-key))))
             ("brave-search" . (:command "npx"
                                         :args ("-y" "@brave/brave-search-mcp-server"
                                                "--transport" "stdio")
                                         :env (:BRAVE_API_KEY ,(get-brave-api-key))))))
  :config (require 'mcp-hub)
  :hook (after-init . mcp-hub-start-all-server))

(use-package superchat
  :straight (:host github :repo "yibie/superchat")
  :after gptel
  :custom
  (superchat-lang "中文")
  (superchat-default-directories '("~/Documents" "~/WorkSpace/")))

(use-package claude-code-ide
  :straight (:host github :repo "manzaltu/claude-code-ide.el")
  :config
  (claude-code-ide-emacs-tools-setup)
  :init
  (leader-def
    "C"  '(:ignore t :which-key "Claude IDE")
    "Cc" '(claude-code-ide-menu :which-key "Popup ClaudeCode menu"))
  )

(use-package minuet
  :straight t
  :defer t
  :bind
  (("M-y" . #'minuet-complete-with-minibuffer) ;; use minibuffer for completion
   ("M-i" . #'minuet-show-suggestion) ;; use overlay for completion
   ("C-c m" . #'minuet-configure-provider)
   :map minuet-active-mode-map
   ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
   ("M-p" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
   ("M-n" . #'minuet-next-suggestion) ;; invoke completion or cycle to previous completion
   ("M-A" . #'minuet-accept-suggestion) ;; accept whole completion
   ;; Accept the first line of completion, or N lines with a numeric-prefix:
   ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
   ("M-a" . #'minuet-accept-suggestion-line)
   ("M-e" . #'minuet-dismiss-suggestion))
  :custom
  (minuet-provider 'openai)
  :hook
  (prog-mode . minuet-auto-suggestion-mode)
  (minuet-active-mode . evil-normalize-keymaps)
  :config
  (setenv "OPENAI_API_KEY" (get-openai-api-key))
  ;; change openai model to gpt-4.1
  (plist-put minuet-openai-options :model "gpt-4.1-mini")
  ;; :init
  (plist-put minuet-openai-options :api-key "OPENAI_API_KEY"))

(use-package aidermacs
  :straight t
  :defer t
  :config
  ;; (setenv "OPENROUTER_API_KEY" (get-openrouter-api-key))
  ;; (setenv "GEMINI_API_KEY" (get-gemini-api-key))
  (setenv "DEEPSEEK_API_KEY" (get-deepseek-api-key))
  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "deepseek/deepseek-reasoner")
  :init
  (leader-def
    "a"  '(:ignore t :which-key "aidermacs")
    "aa" '(aidermacs-transient-menu :which-key "Popup Aidermacs menu")))


(provide 'init-ai)
;;; init-ai.el ends here.
