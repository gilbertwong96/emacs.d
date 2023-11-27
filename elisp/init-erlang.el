;;; init-erlang.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Gilbert's Emacs Configuration

;;; Code:

(use-package erlang
  :straight t
  :mode ("rebar\\.\\(config\\|lock\\)" . erlang-mode)
  :demand t
  :ensure
  :defer t
  :hook
  (erlang-mode . eglot-ensure)
  )

(provide 'init-erlang)
;;; init-erlang.el ends here
