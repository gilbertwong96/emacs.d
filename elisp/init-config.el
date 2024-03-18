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
  :if (eq system-type 'darwin)
  :ensure-system-package yaml-language-server
  :if (eq system-type 'gnu/linux)
  :ensure-system-package (yaml-language-server . "brew install yaml-language-server")
  )

;; Major mode for Hashicorp Configuration Language.
(use-package hcl-mode
  :straight t
  :defer t
  )

(use-package protobuf-mode :straight t :defer t :ensure t)

(use-package terraform-mode
  :straight t
  :if (eq system-type 'darwin)
  :ensure-system-package terraform-ls
  :if (eq system-type 'gnu/linux)
  :ensure-system-package (terraform-ls . "brew install terraform-ls")
  :custom
  (terraform-format-on-save t)
  :hook
  (terraform-mode . eglot-ensure)
  )

(use-package just-mode
  :straight t
  :if (eq system-type 'darwin)
  :ensure-system-package just
  :if (eq system-type 'gnu/linux)
  :ensure-system-package (terraform-ls . "brew install just")
  )

(use-package justl
  :straight t
  :defer t)


(provide 'init-config)
;;; init-config.el ends here
