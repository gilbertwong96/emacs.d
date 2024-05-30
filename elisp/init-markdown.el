;;; Package ---  init-markdown.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Markdown mode for Emacs

;;; Code:

(use-package markdown-mode
  :straight t
  :defer t
  :hook
  (markdown-mode . eglot-ensure)
  :custom
  (markdown-fontify-code-blocks-natively t "Enable code block syntax highlighting")
  (markdown-enable-highlighting-syntax t "Enable obsidian's syntax highlighting")
  :custom-face
  (fixed-pitch ((t (:family "JetBrainsMono Nerd Font"))))
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :ensure-system-package marksman
  )

(provide 'init-markdown)
;;; init-markdown.el ends here
