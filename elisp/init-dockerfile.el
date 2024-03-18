;;; Package --- init-dockerfile.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Settings for Dockerfile

;;; Code:

(use-package dockerfile-mode
  :straight t
  :if (eq system-type 'darwin)
  :ensure-system-package
  ("/Applications/OrbStack.app" . "brew install orbstack --cask")
  )

(provide 'init-dockerfile)
;;; init-dockerfile.el ends here
