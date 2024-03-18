;;; Package --- init-editor.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Enhance editor

;;; Code:

(use-package editorconfig
  :straight t
  :ensure t
  :config
  (editorconfig-mode 1)
  :if (eq system-type 'darwin)
  :ensure-system-package editorconfig
  :if (eq system-type 'gnu/linux)
  :ensure-system-package (editor . "brew install editorconfig")
  )

(use-package flymake-vale
  :straight (:host github :repo "tpeacock19/flymake-vale")
  :ensure t
  :if (eq system-type 'darwin)
  :ensure-system-package vale
  :if (eq system-type 'gnu/linux)
  :ensure-system-package (vale . "brew install vale")
  :hook
  (find-file . flymake-vale-maybe-load)
  (org-mode . flymake-mode)
  )


(provide 'init-editor)
;;; init-editor.el ends here
