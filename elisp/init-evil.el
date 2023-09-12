;;; init-evil.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Install evil packages and declare key bindings

;;; Code:

(use-package evil
  :straight t
  :demand t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :hook
  (after-init . evil-mode)
  :config
  (define-key evil-normal-state-map (kbd "C-h") #'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") #'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") #'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") #'evil-window-right))

  ;; set leader key in normal state
  ;; (evil-set-leader 'normal (kbd "SPC"))

  ;; set local leader key in normal state
  ;; (evil-set-leader 'normal "," t)


(use-package evil-collection
  :straight t
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-escape
  :straight t
  :demand t
  :after evil
  :config
  (evil-escape-mode t)

  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.08))

(use-package accelerate
  :straight t
  :config
  (accelerate evil-previous-line 2)
  )

(provide 'init-evil)
;;; init-evil.el ends here
