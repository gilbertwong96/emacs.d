;;; init-erlang.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Gilbert's Emacs Configuration

;;; Code:

(use-package erlang
  :straight t
  :demand t
  :ensure t
  :defer t
  :hook
  (erlang-mode . eglot-ensure)
  )

(provide 'init-erlang)
;;; init-erlang.el ends here
