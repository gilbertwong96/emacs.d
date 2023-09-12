;;; init-shell.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Gilbert's Emacs Configuration

;;; Code:

(use-package flymake-shellcheck
  :straight t
  :defer t
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(use-package fish-mode
  :straight t
  :defer t)

(provide 'init-shell)
;;; init-shell.el ends here
