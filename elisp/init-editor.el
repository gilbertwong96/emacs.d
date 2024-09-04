;;; Package --- init-editor.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Enhance editor

;;; Code:

(use-package editorconfig
  :straight t
  :ensure t
  :defer t
  :config
  (editorconfig-mode 1)
  :ensure-system-package editorconfig
  )

(use-package flymake-vale
  :if (eq system-type 'darwin)
  :straight (:host github :repo "tpeacock19/flymake-vale")
  :ensure t
  :ensure-system-package vale
  :hook
  (find-file . flymake-vale-maybe-load)
  (org-mode . flymake-mode)
  )


(provide 'init-editor)
;;; init-editor.el ends here
