;;; init-just.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Gilbert's Emacs Configuration

;;; Code:

(use-package just-mode
  :straight t
  :defer t
  :demand t
  )

(use-package justl
  :straight t
  :ensure t
  :defer t)

(provide 'init-just)
;;; init-just.el ends here
