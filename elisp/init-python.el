;;; Package --- init-python.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:

;; Enhance Python Developing

;;; Code:

(use-package python-mode
  :straight t
  :ensure t
  :defer t
  :hook
  (python-ts-mode . (lambda () (setq-local tab-width 4) (setq-local python-indent-offset 4)))
  )

(use-package conda
  :if (eq system-type 'darwin)
  :straight t
  :ensure t
  :defer t
  :hook
  (find-file . (lambda () (when (bound-and-true-p conda-project-env-path)
                       (conda-env-activate-for-buffer))))
  (after-init . (lambda () (conda-env-initialize-interactive-shells)
                  (conda-env-initialize-eshell)
                  (conda-env-autoactivate-mode t)
                  (conda-mode-line-setup)
                  ))
  )


(provide 'init-python)
;;; init-python.el ends here
