;;; Package ---  init-erlang.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Enhance Erlang developing

;;; Code:

(use-package erlang
  :straight t
  :ensure t
  :defer t
  :mode
  ("rebar\\.\\(config\\|lock\\)" . erlang-mode)
  ("elvis\\.config" . erlang-mode)
  :hook
  (erlang-mode . (lambda () (setq truncate-lines t)))
  (eglot-managed-mode . flymake-mode)
  :ensure-system-package erlang)

(provide 'init-erlang)
;;; init-erlang.el ends here
