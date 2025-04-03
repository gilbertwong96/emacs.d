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
  :init
  (leader-def
    "vt" 'multi-vterm-dedicated-open
    )
  (local-leader-def
    "vt" 'multi-vterm-dedicated-toggle
    )
  )

(provide 'init-terminal)
;;; init-terminal.el ends here
