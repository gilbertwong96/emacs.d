;;; init-config.el -*- lexical-binding: t; -*-

;;; Commentary:

;;  Enhance config-mode

;;; Code:

(use-package yaml-mode
  :straight t
  :ensure t
  :hook
  (yaml-mode . eglot-ensure)
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  )

(provide 'init-config)
