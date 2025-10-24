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
  (straight-use-package-by-default nil)
  (straight-check-for-modifications '(watch-files find-when-checking)))

(use-package compile-angel
  :straight t
  :demand t
  :config
  ;; Set `compile-angel-verbose' to nil to disable compile-angel messages.
  ;; (When set to nil, compile-angel won't show which file is being compiled.)
  (setq compile-angel-verbose t)

  ;; Uncomment the line below to compile automatically when an Elisp file is saved
  ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)

  ;; The following directive prevents compile-angel from compiling your init
  ;; files. If you choose to remove this push to `compile-angel-excluded-files'
  ;; and compile your pre/post-init files, ensure you understand the
  ;; implications and thoroughly test your code. For example, if you're using
  ;; the `use-package' macro, you'll need to explicitly add:
  ;; (eval-when-compile (require 'use-package))
  ;; at the top of your init file.
  (push "/init.el" compile-angel-excluded-files)
  (push "/early-init.el" compile-angel-excluded-files)

  ;; A global mode that compiles .el files before they are loaded
  ;; using `load' or `require'.
  (compile-angel-on-load-mode 1))

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
