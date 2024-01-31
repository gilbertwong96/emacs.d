;;; Package ---  init-erlang.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Gilbert's Emacs Configuration

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
  )

(provide 'init-erlang)
;;; init-erlang.el ends here
