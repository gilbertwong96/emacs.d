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
  (defconst leader "SPC")
  (defconst local-leader ",")
  (general-create-definer leader-def
    ;; :prefix my-leader
    :prefix leader)
  (general-create-definer local-leader-def
  ;; :prefix my-local-leader
    :prefix local-leader)

  (leader-def
    :keymaps 'normal
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
    "hv"  'describe-variable
    "hf"  'describe-function
    "pf"  'project-find-file
    "pp"  'project-switch-project
    "me"  'manual-entry
    "mm"  'info-display-manual
    )
 )

;  (defconst leader "SPC")
;  (general-create-definer leader-def
;    :prefix "<SPC>")
;  ;; setting keybindings for basic edit operations
;  (leader-def "fs" 'evil-save)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
