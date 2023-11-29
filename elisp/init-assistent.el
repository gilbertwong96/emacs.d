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
  (define-key copilot-completion-map (kbd "M-j") 'copilot-accept-completion)
  (setq copilot-max-char 1000000)
  )

(use-package imenu-list
  :straight t
  :ensure t
  :after general
  :config
  (leader-def
    "'" 'imenu-list-smart-toggle)
  )

(provide 'init-assistent)
;;; init-assistent.el ends here
