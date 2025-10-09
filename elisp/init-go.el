;;; Package --- init-go.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Enhance golang development

;;; Code:

(use-package go-mode
  :straight t
  :hook
  (go-mode . eglot-ensure))

(provide 'init-go)
;;; init-go.el ends here
