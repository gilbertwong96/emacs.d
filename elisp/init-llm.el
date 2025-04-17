;;; Package --- init-llm.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Integrate LLM Tools

;;; Code:


(defun get-openrouter-api-key ()
  "Get OpenRouter API Key from 1password."
  (string-trim
   (shell-command-to-string "op read op://Personal/OpenRouter-Gemini/credential")))

(defun get-gemini-api-key ()
  "Get OpenRouter API Key from 1password."
  (string-trim
   (shell-command-to-string "op read 'op://Personal/Google AI Studio API/credential'")))

(use-package aidermacs
  :straight t
  :defer t
  :config
  (setenv "OPENROUTER_API_KEY" (get-openrouter-api-key))
  ;; (setenv "GEMINI_API_KEY" (get-gemini-api-key))
  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "openrouter/google/gemini-2.5-pro-exp-03-25:free")
  :init
  (leader-def
    "a"  '(:ignore t :which-key "aidermacs")
    "aa" '(aidermacs-transient-menu :which-key "Popup Aidermacs menu")))


(provide 'init-llm)
;;; init-llm.el ends here.
