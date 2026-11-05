;;; Package --- init-java.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Enhance Java developing

;;; Code:

(use-package cc-mode
  :straight (:type built-in)
  :hook
  (java-mode . eglot-ensure))

(provide 'init-java)
;;; init-java.el ends here
