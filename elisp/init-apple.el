;;; Package --- init-apple.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Apple ecosystem development utility

;;; Code:


(use-package swift-mode
  :if (eq system-type 'darwin)
  :straight t
  :mode "\\.swift\\'"
  :interpreter "swift"
)


;;;
(provide 'init-apple)
;;; init-apple.el ends here
