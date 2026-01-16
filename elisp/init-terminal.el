;;; Package --- init-terminal.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Gilbert's Emacs Configuration

;;; Code:

(use-package multi-vterm
  :straight t
  :defer t
  :config
  (setq multi-vterm-dedicated-window-height 50)
  (setq multi-vterm-dedicated-window-height-percent 30)
  :custom
  (vterm-max-scrollback 100000)
  (vterm-timer-delay 0.01)
  :config
  (define-key vterm-mode-map (kbd "C-c C-g") 'vterm-send-C-g)
  :init
  (leader-def
    "v" '(:ignore t :which-key "vterm keymaps")
    "vt" 'multi-vterm-dedicated-open
    "ve" 'vterm-send-escape
    "vg" 'vterm-send-C-g
    "vc" 'vterm-send-C-c)
  (local-leader-def
    "vt" 'multi-vterm-dedicated-toggle))

(provide 'init-terminal)
;;; init-terminal.el ends here
