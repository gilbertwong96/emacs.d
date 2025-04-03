;;; Package --- init-golang.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Gilbert's Emacs Configuration for golang

(use-package go-mode
  :straight t
  :hook
  (go-mode . eglot-ensure))

;;; Code:
(provide 'init-golang)
;;; init-golang.el ends here
