;;; Package --- init-keybindings.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Set Keybindings

;;; Code:

(use-package which-key
  :straight t
  :defer t
  :custom
  (which-key-allow-evil-operators nil)
  :init
  (which-key-mode)
  :config
  (which-key-setup-side-window-bottom)
  )

(use-package general
  :straight t
  :after which-key
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
    "SPC" '(execute-extended-command :which-key "Commands")
    "f"   '(:ignore t :which-key "files")
    "ff"  '(find-file :which-key "Find")
    "fs"  '(save-buffer :which-key "Save")
    "fr"  '(recentf :which-key "Recent")
    "q"   '(:ignore t :which-key "leave")
    "qr"  '(restart-emacs :which-key "Restart")
    "qq"  '(save-buffers-kill-emacs :which-key "Quit")
    "w"   '(:ignore t :which-key "window")
    "wm"  '(maximize-window :which-key "Maximize")
    "w="  '(balance-windows :which-key "Balance")
    "wd"  '(delete-window :which-key "Delete")
    "h"   '(:ignore t :which-key "help")
    "hd"  '(:ignore t :which-key "describe")
    "hdv" '(describe-variable :which-key "Variable")
    "hdf" '(describe-function :which-key "Function")
    "hdk" '(describe-key :which-key "Key")
    "hdp" '(describe-package :which-key "Package")
    "p"   '(:ignore t :which-key "project")
    "pf"  '(project-find-file :which-key "Find File")
    "pp"  '(project-switch-project :which-key "Switch Project")
    "pr"  '(project-query-replace-regexp :which-key "Query Replace Regex")
    "ps"  '(projectile-save-project-buffers :which-key "Save Project Buffers")
    "m"   '(:ignore t :which-key "man")
    "me"  '(manual-entry :which-key "Unix Command")
    "mm"  '(info-display-manual :which-key "Emacs Package")
    )
 )

;; Enhance Imenu
(use-package imenu-list
  :straight t
  :ensure t
  :after general
  :config
  (leader-def
    "'" '(imenu-list-smart-toggle :which-key "Toggle Imenu List"))
  )

(provide 'init-keybindings)
;;; init-keybindings.el ends here
