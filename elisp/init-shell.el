;;; package --- summary init-shell.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Gilbert's Emacs Configuration

;;; Code:

(use-package flymake-shellcheck
  :straight t
  :defer t
  :commands flymake-shellcheck-load
  :hook
  (sh-mode . flymake-shellcheck-load)
  (bash-ts-mode . flymake-shellcheck-load)
  :if (eq system-type 'darwin)
  :ensure-system-package shellcheck
  :if (eq system-type 'gnu/linux)
  :ensure-system-package (shellcheck . "brew install shellcheck")
  )

(use-package fish-mode
  :straight t
  :defer t)

(provide 'init-shell)
;;; init-shell.el ends here
