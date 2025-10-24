;;; Package --- init-git.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Gilbert's Emacs Configuration

;;; Code:

(use-package magit
  :straight t
  :defer t
  :ensure-system-package git
  :config
  (transient-append-suffix 'magit-push "-n"
    '("-s" "Skip CI" "-o ci.skip"))
  :init
  (leader-def
    "gs" 'magit-status
    "gb" 'magit-blame-addition
    "gc" 'magit-clone
    )
  )

(use-package magit-lfs
  :straight t
  :defer t)

(use-package git-gutter
  :straight t
  :defer t
  :custom
  (git-gutter:update-interval 1 "set update interval 1 seconds")
  (git-gutter:modified-sign "│")
  (git-gutter:added-sign "│")
  (git-gutter:deleted-sign "│")
  :hook
  (prog-mode . git-gutter-mode)
  )

(use-package magit-todos
  :straight t
  :after magit
  :config
  (magit-todos-mode 1))

(provide 'init-git)
;;; init-git.el ends here
