;;; Package --- init-java.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Gilbert's Emacs Configuration

;;; Code:

(use-package java-mode
  :straight t
  :defer t
  :hook
  (java-mode . eglot-ensure)
  :ensure-system-package java)

(add-hook 'java-mode #'eglot-ensure)

(provide 'init-java)
;;; init-java.el ends here
