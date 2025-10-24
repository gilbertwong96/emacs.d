;;; package --- Summary init.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Gilbert's Emacs Configuration

;;; Code:

(use-package benchmark-init
  :straight t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package straight
  :custom
  (straight-disable-native-compile nil)
  (straight-use-package-by-default nil)
  (straight-check-for-modifications '(watch-files find-when-checking)))

(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize))

(use-package project :straight (:type built-in) :defer t)
(use-package flymake :straight (:type built-in) :defer t)
(use-package xref :straight (:type built-in) :defer t)


(use-package use-package-ensure-system-package
  :if (eq system-type 'darwin)
  :straight (:host github :repo "waymondo/use-package-ensure-system-package"))

;; Place user settings to elisp dir
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))


;; Basic setting for editor
(use-package init-theme)
(use-package init-settings)
(use-package init-keybindings)
;; Knowledge Management System
(use-package init-org)
(use-package init-treemacs)
(use-package init-autocomplete)
(use-package init-vertico)
(use-package init-evil)
(use-package init-ai)
(use-package init-terminal)

;; Develop Kits
(use-package init-git)
(use-package init-lsp)
(use-package init-docker)
(use-package init-config)
(use-package init-elisp)
(use-package init-shell)
(use-package init-erlang)
(use-package init-haskell)
(use-package init-go)
(use-package init-markdown)
(use-package init-python)
(use-package init-rust)
(use-package init-elixir)
(use-package init-treesit)
(use-package init-db)
(use-package init-flutter)


;; Development for Apple Platform
(use-package init-apple)
(use-package init-web)

;; Enhance editor
(use-package init-editor)

;; Reading
(use-package init-reading)

(provide 'init)
;;; init.el ends here
