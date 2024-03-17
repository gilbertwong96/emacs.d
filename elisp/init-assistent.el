;;; Package --- init-assistent.el -*- lexical-binding: t; -*-

;;; Commentary:

;; AI Assistent

;;; Code:

(use-package copilot
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
