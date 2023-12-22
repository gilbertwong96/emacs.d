;;; init-config.el -*- lexical-binding: t; -*-

;;; Commentary:

;;  Enhance config-mode

;;; Code:

(use-package yaml-mode
  :straight t
  :defer t
  :hook
  (yaml-mode . eglot-ensure)
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  )


;; Major mode for Hashicorp Configuration Language.
(use-package hcl-mode
  :straight t)

(use-package terraform-mode
  :straight t
  :custom
  (terraform-format-on-save t)
  :hook
  (terraform-mode . eglot-ensure)
  )

(provide 'init-config)
