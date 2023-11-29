;;; init-org.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Gilbert's Emacs Configuration

;;; Code:

(use-package org
  :straight t
  :defer t
  :custom
  (org-todo-keywords '((sequence "TODO" "DOING" "DONE")))
  )

(use-package org-roam
  :straight t
  :ensure t
  :defer t
  :custom
  (org-roam-directory "~/RoamNotes")
  (org-roam-complete-everywhere t)
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M %p>: %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  ;; :bind
  ;; (("C-c n l" . org-roam-buffer-toggle)
  ;;  ("C-c n f" . org-roam-node-find)
  ;;  ("C-c n i" . org-roam-node-insert)
  ;;  :map org-roam-dailies-map
  ;;  ("Y" . org-roam-dailies-capture-yesterday)
  ;;  ("T" . org-roam-dailies-capture-tomorrow)
  ;;  )
  ;; :bind-keymap
  ;; ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-setup)
  :init
  (leader-def
    "nf" 'org-roam-node-find
    "ni" 'org-roam-node-insert
    "ndY" 'org-roam-dailies-capture-yesterday
    "ndT" 'org-roam-dailies-capture-tomorrow
    )

  (leader-def
    :keymaps 'org-roam-mode-map
    "nl" 'org-roam-buffer-toggle
    )
  )

(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(provide 'init-org)
;;; init-org.el ends here
