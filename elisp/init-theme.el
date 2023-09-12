;;; init-theme.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Customize Gilbert's Emacs theme

;;; Code:

(use-package monokai-theme
  :straight t
  :ensure t
  :config
  (load-theme 'monokai t)
  )


(provide 'init-theme)
;;; init-theme.el ends here
