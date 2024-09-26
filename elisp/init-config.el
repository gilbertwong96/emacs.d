;;; Package --- init-config.el -*- lexical-binding: t; -*-

;;; Commentary:

;;  Enhance config-mode

;;; Code:

(use-package yaml-mode
  :straight t
  :hook
  (yaml-mode . eglot-ensure)
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  :ensure-system-package yaml-language-server
  )

;; Major mode for Hashicorp Configuration Language.
(use-package hcl-mode
  :straight t
  :defer t
  )

(use-package protobuf-mode :straight t :defer t :ensure t)

(use-package terraform-mode
  :straight t
  :ensure-system-package terraform-ls
  :custom
  (terraform-format-on-save t)
  :hook
  (terraform-mode . eglot-ensure)
  )

(use-package just-mode
  :straight t
  :ensure-system-package just
  )

(use-package justl
  :straight t
  :defer t)

(use-package nix-mode
  :straight t
  :mode "\\.nix\\'")

(use-package direnv
  :straight t
  :config
  (direnv-mode))


(provide 'init-config)
;;; init-config.el ends here
