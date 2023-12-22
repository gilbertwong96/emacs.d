;;; init-erlang.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Gilbert's Emacs Configuration

;;; Code:

(use-package erlang
  :straight t
  :mode ("rebar\\.\\(config\\|lock\\)" . erlang-mode)
  :ensure
  :defer t
  :hook
  (erlang-mode . eglot-ensure)
  (erlang-mode . flymake-mode)
  )

(provide 'init-erlang)
;;; init-erlang.el ends here
