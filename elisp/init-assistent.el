;;; init-assistent.el -*- lexical-binding: t; -*-

;;; Commentary:

;; AI Assistent

;;; Code:

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :defer t
  :custom
  (copilot-idle-delay 1 "Set Copilot-idle-delay to 1s")
  :hook
  (prog-mode . copilot-mode)
  :config
  (define-key copilot-completion-map (kbd "C-j") 'copilot-accept-completion)
  (setq copilot-max-char 1000000)
  )

(provide 'init-assistent)
;;; init-assistent.el ends here
