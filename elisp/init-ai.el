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

(use-package pi-coding-agent
  :straight t
  :init (defalias 'pi 'pi-coding-agent)
  :config
  ;; pi-coding-agent leader keybindings under SPC a / C-c a
  (leader-def
    "a"  '(:ignore t :which-key "ai")
    ;; Top-level actions
    "aa" '(pi-coding-agent                 :which-key "Start Session")
    "at" '(pi-coding-agent-toggle          :which-key "Toggle Visibility")
    "am" '(pi-coding-agent-menu            :which-key "Open Menu")
    ;; Input actions (when in input buffer)
    "as" '(pi-coding-agent-send            :which-key "Send Prompt")
    "ak" '(pi-coding-agent-abort           :which-key "Abort Streaming")
    "aS" '(pi-coding-agent-queue-steering  :which-key "Queue Steering")
    "af" '(pi-coding-agent-queue-followup  :which-key "Queue Follow-up")
    ;; Session management
    "an" '(pi-coding-agent-new-session     :which-key "New Session")
    "ar" '(pi-coding-agent-resume-session  :which-key "Resume Session")
    "aR" '(pi-coding-agent-reload          :which-key "Reload Session")
    "aN" '(pi-coding-agent-set-session-name :which-key "Set Session Name")
    "ae" '(pi-coding-agent-export-html     :which-key "Export to HTML")
    "aq" '(pi-coding-agent-quit            :which-key "Quit Session")
    ;; Context
    "ac" '(pi-coding-agent-compact         :which-key "Compact Context")
    "aF" '(pi-coding-agent-fork            :which-key "Fork Session")
    ;; Model & thinking
    "aM" '(pi-coding-agent-select-model    :which-key "Select Model")
    "aT" '(pi-coding-agent-cycle-thinking  :which-key "Cycle Thinking Level")
    "ad" '(pi-coding-agent-toggle-thinking-display          :which-key "Toggle Thinking Display")
    "aD" '(pi-coding-agent-toggle-default-thinking-display  :which-key "Toggle Default Thinking Display")
    ;; Info
    "ai" '(pi-coding-agent-session-stats   :which-key "Session Stats")
    "ap" '(pi-coding-agent-process-info    :which-key "Process Info")
    "ay" '(pi-coding-agent-copy-last-message :which-key "Copy Last Message")
    "ag" '(pi-coding-agent-refresh-commands :which-key "Refresh Commands")
    ;; Sub-menus
    "ax" '(pi-coding-agent-extensions-menu :which-key "Extensions Menu")
    "aK" '(pi-coding-agent-skills-menu     :which-key "Skills Menu")
    "aL" '(pi-coding-agent-templates-menu  :which-key "Templates Menu")
    "a!" '(pi-coding-agent-run-custom-command :which-key "Run Custom Command")
    ))

(provide 'init-ai)
;;; init-ai.el ends here.
