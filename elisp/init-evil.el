;;; Package --- init-evil.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Install evil packages and declare key bindings

;;; Code:

(use-package evil
  :straight t
  :custom
  (evil-want-keybinding nil)
  (evil-want-integration t)
  (evil-undo-system 'undo-redo)
  :init
  (setq-default evil-disable-insert-state-bindings t)
  ;; (setq evil-want-keybinding nil)
  :hook
  (after-init . evil-mode)
  :after general
  :config
  (defalias #'forward-evil-word #'forward-evil-symbol)
  ;; make evil-search-word look for symbol rather than word boundaries
  (setq-default evil-symbol-word-search t)
  (leader-def
    "b"  '(:ignore t          :which-key "buffers")
    "bd" '(evil-delete-buffer :which-key "Delete Buffer")
    "T"  '(:ignore T          :which-key "tabs")
    "Tn" '(tab-new            :which-key "New Tab")
    "Td" '(tab-close          :which-key "Close Tab")
    "Tr" '(tab-rename         :which-key "Rename Tab")
    )
  (cond ((eq system-type 'darwin)
         (bind-keys :package evil :map evil-motion-state-map
                    ("s-h" . evil-window-left)
                    ("s-j" . evil-window-down)
                    ("s-k" . evil-window-up)
                    ("s-l" . evil-window-right)
                    ("s-t" . tab-new)
                    ("s-]" . tab-next)
                    ("s-[" . tab-previous)
                    ("M-<right>" . enlarge-window-horizontally)
                    ("M-<left>" . shrink-window-horizontally)
                    ("M-<up>" . enlarge-window)
                    ("M-<down>" . shrink-window)))
        ((eq system-type 'gnu/linux)
         (bind-keys :package evil :map evil-motion-state-map
                    ("C-h" . evil-window-left)
                    ("C-j" . evil-window-down)
                    ("C-k" . evil-window-up)
                    ("C-l" . evil-window-right)
                    ("M-]" . tab-next)
                    ("M-[" . tab-previous)
                    ("M-<right>" . enlarge-window-horizontally)
                    ("M-<left>" . shrink-window-horizontally)
                    ("M-<up>" . enlarge-window)
                    ("M-<down>" . shrink-window))
         )
        )
  )

(use-package evil-collection
  :straight t
  :after evil
  :custom
  (global-evil-collection-unimpaired-mode t)
  :init
  (evil-collection-init)
  :config
  (defun ignore-dired-space ()
    (evil-collection-define-key 'normal 'dired-mode-map
      " " 'nil)
    )
  (add-hook 'dired-mode-hook 'ignore-dired-space)
  )

(use-package evil-escape
  :straight t
  :after evil
  :custom
  (evil-escape-mode t)
  (evil-escape-key-sequence "jk")
  (evil-escape-delay 0.08))

(use-package evil-surround
  :straight t
  :after evil
  :config
  (global-evil-surround-mode t))

(if (not (display-graphic-p))
    ;; Code to run in terminal mode (optional)
    (use-package navigate
      :after evil
      :straight (:host github :repo "keith/evil-tmux-navigator"))
  )


(provide 'init-evil)
;;; init-evil.el ends here
