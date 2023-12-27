;;; init-evil.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Install evil packages and declare key bindings

;;; Code:

(use-package evil
  :straight t
  :init
  (setq-default evil-disable-insert-state-bindings t)
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
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
        ("s-h" . evil-window-left)
        ("s-j" . evil-window-down)
        ("s-k" . evil-window-up)
        ("s-l" . evil-window-right)
        ("s-t" . tab-new)
        ("s-]" . tab-next)
        ("s-[" . tab-previous)
        )
  )

(use-package evil-collection
  :straight t
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-escape
  :straight t
  :after evil
  :config
  (evil-escape-mode t)

  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.08))

(use-package evil-surround
  :straight t
  :ensure t
  :config
  (global-evil-surround-mode 1))

(provide 'init-evil)
;;; init-evil.el ends here
