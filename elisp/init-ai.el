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

(provide 'init-ai)
;;; init-ai.el ends here.
