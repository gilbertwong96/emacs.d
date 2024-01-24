;;; Package ---  init-erlang.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Gilbert's Emacs Configuration

;;; Code:

(use-package erlang
  :straight t
  :mode
  ("rebar\\.\\(config\\|lock\\)" . erlang-mode)
  ("elvis\\.config" . erlang-mode)
  :ensure t
  :hook
  (erlang-mode . eglot-ensure)
  ;; (erlang-mode . flymake-mode)
  (erlang-mode . (lambda () (setq truncate-lines t)))
  )

(provide 'init-erlang)
;;; init-erlang.el ends here
