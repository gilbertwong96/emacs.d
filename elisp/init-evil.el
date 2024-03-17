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
    "bd" 'evil-delete-buffer)
  :bind
  (:map evil-motion-state-map
        ("M-h" . evil-window-left)
        ("M-j" . evil-window-down)
        ("M-k" . evil-window-up)
        ("M-l" . evil-window-right)
        ("M-t" . tab-new)
        ("M-]" . tab-next)
        ("M-[" . tab-previous)
        ("M-<right>" . enlarge-window-horizontally)
        ("M-<left>" . shrink-window-horizontally)
        ("M-<up>" . enlarge-window)
        ("M-<down>" . shrink-window))
  )

(use-package evil-collection
  :straight t
  :after evil
  :ensure t
  :custom
  (global-evil-collection-unimpaired-mode t)
  :hook
  (evil-mode . evil-collection-setup)
  :config
  ;; (evil-collection-init)
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
  :ensure t
  :config
  (global-evil-surround-mode))

(provide 'init-evil)
;;; init-evil.el ends here
