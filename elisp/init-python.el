;;; Package --- init-python.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:

;; Enhance Python Developing

;;; Code:


(use-package conda-project
  :straight (:host github :repo "gilbertwong96/conda-project.el" :files ("*.el"))
  :hook
  (python-ts-mode . conda-project-env-autoactivate-mode)
  (python-ts-mode . (lambda () (setq-local tab-width 4) (setq-local python-indent-offset 4)))
  :init
  (local-leader-def
    "c" '(:ignore t :which-key "conda-project")
    "cI" '(conda-project-init :which-key "Init a new project")
    "cl" '(conda-project-lock :which-key "Lock environment")
    "cc" '(conda-project-check :which-key "Check inconsistencies or errors")
    "ci" '(conda-project-install :which-key "Install packages")
    "ca" '(conda-project-add :which-key "Add packages")
    "cr" '(conda-project-remove :which-key "Remove packages")
    "cC" '(conda-project-clean :which-key "Clean environment")
    "cR" '(conda-project-run :which-key "Run command")
    )
  )

(provide 'init-python)
;;; init-python.el ends here
