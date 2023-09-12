;;; init-markdown.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Markdown mode for emacs

;;; Code:

(use-package markdown-mode
  :straight t
  :ensure t
  :defer t
  :hook
  (markdown-mode . eglot-ensure)
  :custom
  (markdown-fontify-code-blocks-natively t "Enable code block syntax highlighting")
  (markdown-enable-highlighting-syntax t "Enable obsidian's syntax highlighting")
  :custom-face
  (fixed-pitch ((t (:family "JetBrainsMono Nerd Font"))))
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; (use-package poly-markdown
;;   :straight t
;;   :ensure t
;;   :defer t
;;   :hook
;;   (poly-markdown-mode . markdown-mode)
;;   )

(provide 'init-markdown)
;;; init-markdown.el ends here
