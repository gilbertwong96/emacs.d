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
  :init
  (leader-def
    "v" '(:ignore t :which-key "vterm keymaps")
    "vt" 'multi-vterm-dedicated-open
    "ve" 'vterm-send-escape)
  (local-leader-def
    "vt" 'multi-vterm-dedicated-toggle))

(use-package eat
  :straight (eat :type git
                 :host codeberg
                 :repo "akib/emacs-eat"
                 :files ("*.el" ("term" "term/*.el") "*.texi"
                         "*.ti" ("terminfo/e" "terminfo/e/*")
                         ("terminfo/65" "terminfo/65/*")
                         ("integration" "integration/*")
                         (:exclude ".dir-locals.el" "*-tests.el")))
  :config
  ;; Set up proper encoding
  (add-hook 'eat-mode-hook
            (lambda ()
              ;; Ensure UTF-8 encoding
              (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)
              ;; Set locale environment variables
              (setq-local process-environment
                          (cons "LANG=en_US.UTF-8"
                                (cons "LC_ALL=en_US.UTF-8"
                                      process-environment))))))

(provide 'init-terminal)
;;; init-terminal.el ends here
