;;; init-lsp.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Language Server Protocol Settings

;;; Code:


(use-package eglot
  :straight t
  :ensure t
  :defer t
  :init
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(provide 'init-lsp)
