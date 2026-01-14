;;; package --- init-org.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Gilbert's Emacs Configuration

;;; Code:

(use-package simple-httpd
  :straight t
  :defer t)

(use-package mermaid-mode
  :straight t
  :after org
  :custom
  (mermaid-mmdc-location (executable-find "mmdc"))
  )

(use-package ob-mermaid
  :straight t
  :after (org mermaid-mode)
  :config
  (setq ob-mermaid-cli-path (executable-find "mmdc"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((mermaid . t)
     (scheme . t)
     (emacs-lisp . t)))
  )

(use-package verb
  :after org
  :straight t)

(use-package org
  :straight t
  :custom
  (org-todo-keywords '((sequence "TODO" "DOING" "DONE")))
  ;; (org-todo-keywords
  ;;  '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
  :hook
  (org-mode . corfu-mode)
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  (setq org-latex-create-formula-image-program 'dvisvgm)
  (plist-put org-format-latex-options :scale 2.0)
  :init
  (leader-def
    "o"  '(:ignore t :which-key "org")
    "oa" '(org-agenda :which-key "Agenda")
    "ol" '(consult-org-agenda :which-key "List Agenda"))
  )

(use-package org-roam
  :straight t
  :defer t
  :custom
  (org-hide-emphasis-markers t)
  (org-roam-directory "~/RoamNotes")
  (org-roam-complete-everywhere t)
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M %p>: %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  :config
  (require 'org-tempo)
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode)
  (org-roam-setup)
  ;; Build the agenda list the first time for the session
  (my/org-roam-refresh-agenda-list)

  (add-to-list 'org-after-todo-state-change-hook
               (lambda ()
                 (when (equal org-state "DONE")
                   (my/org-roam-copy-todo-to-today))))

  :init
  (leader-def
    "n"   '(:ignore t :which-key "org roam")
    "nf"  '(org-roam-node-find :which-key "Find Node")
    "nb"  '(my/org-roam-capture-inbox :which-key "Capture Inbox")
    "ni"  '(org-roam-node-insert :which-key "Insert Node")
    "nI"  '(org-roam-node-insert-immediate :which-key "New Node")
    "nL"  '(my/org-roam-find-life :which-key "Find Life")
    "np"  '(my/org-roam-find-project :which-key "Find Project")
    "ns"  '(my/org-roam-find-study :which-key "Find Study")
    "nc"  '(:ignore t :which-key "capture")
    "ncp" '(my/org-roam-capture-project-task :which-key "Project Task")
    "ncs" '(my/org-roam-capture-study-task :which-key "Study Task")
    "ncl" '(my/org-roam-capture-life-task :which-key "Life Task")
    "nd"  '(:ignore t :which-key "daily")
    "ndY" '(org-roam-dailies-capture-yesterday :which-key "Capture Yesterday")
    "ndT" '(org-roam-dailies-capture-tomorrow :which-key "Capture Tomorrow")
    "ndt" '(org-roam-dailies-capture-today :which-key "Capture Today")
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
  :defer t
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t)
  :init
  (leader-def
    :keymaps 'org-roam-mode-map
    "nu" 'org-roam-ui-open
    )
  )


(use-package org-modern
  :straight t
  :after org
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))

;;==============================================================================
;; Define functions
;;==============================================================================

(autoload 'org-roam-capture- "org-roam-capture")
(autoload 'org-roam-node-create "org-roam-node")
(autoload 'org-roam-node-read "org-roam-node")
(autoload 'org-roam-node-insert "org-roam-node")
(autoload 'org-roam-node-tags "org-roam-node")
(autoload 'org-roam-node-file "org-roam-node")
(autoload 'org-roam-node-find "org-roam-node")
(autoload 'org-roam-node-list "org-roam-node")
(autoload 'org-capture-get "org-capture")
(autoload 'org-roam-dailies--capture "org-roam-dailies")
(autoload 'org-refile "org-refile")

;;;###autoload
(defun org-roam-node-insert-immediate (arg &rest args)
  "Create a new node and insert a link in the current document.
The node is from the  `ARG' or `ARGS', and this operation will not
open the new node’s buffer."
  (interactive "P")
  (let ((args (push arg args)))
    (apply #'org-roam-node-insert args)))

;;;###autoload
(defun my/org-roam-filter-by-tag (tag-name)
  "Filter roam notes with specific `TAG-NAME'."
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

;;;###autoload
(defun my/org-roam-list-notes-by-tag (tag-name)
  "List roam notes with specific `TAG-NAME'."
  (mapcar #'org-roam-node-file
          (seq-filter
           (my/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defvar org-agenda-files nil
  "List of files to be used for agenda.") ;; dynamically scoped param

(defun my/org-roam-refresh-agenda-list ()
  "Refresh org agenda files with tagged Org Roam notes.
tags like: Project, Study, Life"
  (interactive)
  (setq org-agenda-files
        (append (my/org-roam-list-notes-by-tag "Project")
                (my/org-roam-list-notes-by-tag "Study")
                (my/org-roam-list-notes-by-tag "Life")
                )))

(defvar org-note-abort nil) ; dynamically scoped

;;;###autoload
(defun my/org-roam-project-finalize-hook ()
  "Add the captured project file to `org-agenda' files.
Only take effect when the capture was not aborted."
  ;; Remove the hook since it was added temporarily
  (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Add project file to the agenda list if the capture was confirmed
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

;;;###autoload
(defun my/org-roam-find-project ()
  "Find roam nodes of projects."
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Select a project file to open, creating it if necessary
  (org-roam-node-find
   nil
   nil
   (my/org-roam-filter-by-tag "Project")
   nil
   :templates
   '(("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+category: Project\n#+filetags: Project")
      :unnarrowed t)))
  )

;;;###autoload
(defun my/org-roam-find-study ()
  "Find roam nodes of study plan."
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Select a project file to open, creating it if necessary
  (org-roam-node-find
   nil
   nil
   (my/org-roam-filter-by-tag "Study")
   nil
   :templates
   '(("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+category: Study\n#+filetags: Study")
      :unnarrowed t)))
  )

;;;###autoload
(defun my/org-roam-find-life()
  "Find roam nodes of life plan."
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Select a project file to open, creating it if necessary
  (org-roam-node-find
   nil
   nil
   (my/org-roam-filter-by-tag "Life")
   nil
   :templates
   '(("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+category: Life\n#+filetags: Life")
      :unnarrowed t)))
  )

;;;###autoload
(defun my/org-roam-capture-inbox ()
  "Capture items into Inbox."
  (interactive)
  (org-roam-capture-
   :node (org-roam-node-create)
   :templates '(("i" "inbox" plain "* %?"
                 :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

;;;###autoload
(defun my/org-roam-capture-study-task ()
  "Capture study tasks."
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Capture the new task, creating the project file if necessary
  (org-roam-capture-
   :node (org-roam-node-read
          nil
          (my/org-roam-filter-by-tag "Study"))
   :templates '(("p" "study" plain "** TODO %?"
                 :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                        "#+title: ${title}\n#+category: Study\n#+filetags: Study"
                                        ("Tasks"))))))

;;;###autoload
(defun my/org-roam-capture-life-task ()
  "Capture life schedules."
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Capture the new task, creating the project file if necessary
  (org-roam-capture-
   :node (org-roam-node-read
          nil
          (my/org-roam-filter-by-tag "Life"))
   :templates '(("p" "life" plain "** TODO %?"
                 :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                        "#+title: ${title}\n#+category: Life\n#+filetags: Life"
                                        ("Tasks"))))))

(defvar org-refile-keep nil
  "Non-nil means `org-refile' will copy instead of refile.")

(defvar org-after-refile-insert-hook nil
  "Hook run after `org-refile' has inserted its stuff at the new location.
Note that this is still *before* the stuff will be removed from
the *old* location.")

;;;###autoload
(defun my/org-roam-copy-todo-to-today ()
  "Copy todo task to today roam note."
  (interactive)
  (let ((org-refile-keep t) ;; Set this to nil to delete the original!
        ;; (org-roam-dailies-capture-templates
        ;;  '(("t" "tasks" entry "%?"
        ;;     :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
        (org-after-refile-insert-hook #'save-buffer)
        today-file
        pos)
    (save-window-excursion
      (org-roam-dailies--capture (current-time) t)
      (setq today-file (buffer-file-name))
      (setq pos (point)))

    ;; Only refile if the target file is different than the current file
    (unless (equal (file-truename today-file)
                   (file-truename (buffer-file-name)))
      (org-refile nil nil (list "Tasks" today-file nil pos)))))

(provide 'init-org)
;;; init-org.el ends here
