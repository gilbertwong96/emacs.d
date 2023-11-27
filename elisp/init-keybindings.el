;;; init-keybindings.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Set Keybindings

;;; Code:

(use-package which-key
  :straight t
  :defer t
  :init
  (which-key-mode)
  :config
  (which-key-setup-side-window-bottom)
  )

(use-package general
  :straight t
  :ensure t
  :config
  (general-auto-unbind-keys)
  (defconst evil-leader "SPC")
  (defconst emacs-leader "C-c")
  (defconst evil-local-leader ",")
  (defconst emacs-local-leader "C-c l")
  (general-create-definer leader-def
    :states '(normal insert emacs)
    :prefix evil-leader
    :non-normal-prefix emacs-leader
    :prefix-command 'leader-prefix-command
    :prefix-map 'leader-prefix-map
    )
  (general-create-definer local-leader-def
    :states '(normal insert emacs)
    :prefix evil-local-leader
    :non-normal-prefix emacs-local-leader
    :prefix-command 'local-leader-prefix-command
    :prefix-map 'local-leader-prefix-map
    )

  (leader-def
    "SPC" 'execute-extended-command
    "ff"  'find-file
    "fs"  'save-buffer
    "bb"  'consult-buffer
    "bk"  'kill-buffer
    "qr"  'restart-emacs
    "qq"  'save-buffers-kill-emacs
    "wm"  'maximize-window
    "w="  'balance-windows
    "rf"  'recentf
    "wd"  'delete-window
    "hdv" 'describe-variable
    "hdf" 'describe-function
    "hdk" 'describe-key
    "hdp" 'describe-package
    "pf"  'project-find-file
    "pp"  'project-switch-project
    "pr"  'project-query-replace-regexp
    "ps"  'projectile-save-project-buffers
    "me"  'manual-entry
    "mm"  'info-display-manual
    )
 )

(provide 'init-keybindings)
;;; init-keybindings.el ends here
