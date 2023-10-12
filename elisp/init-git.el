;;; init-git.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Gilbert's Emacs Configuration

;;; Code:

(use-package magit
  :straight t
  :ensure t
  :defer t
  :init
  (leader-def
    :keymaps 'normal
    "gs" 'magit-status
    "gb" 'magit-blame-addition
    ))

(use-package git-gutter
  :straight t
  :ensure t
  :defer t
  :custom
  (git-gutter:update-interval 5 "set update interval 5 seconds")
  :hook
  (prog-mode . git-gutter-mode)
  )

(use-package git-gutter-fringe
  :straight t
  :ensure t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)
  )

(use-package magit-todos
  :straight t
  :defer t
  :ensure t)

(provide 'init-git)
;;; init-git.el ends here
