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
  :ensure-system-package marksman
  :config
  (local-leader-def
    :keymaps 'markdown-mode-map
    "m"  '(:ignore t :which-key "mermaid")
    "mp" '(markdown-mermaid-preview :which-key "Preview diagram at point")
    "p"  '(org-markdown-preview-mode :which-key "Live preview")
    "P"  '(org-markdown-preview-browse-preview :which-key "Reopen preview"))
  )

(use-package markdown-mermaid
  :straight t
  :defer t
  :config
  (define-advice markdown-mermaid--compile
      (:override (mermaid-code) render-svg)
    "Compile MERMAID-CODE to SVG so previews stay sharp at any zoom.
HTML labels are disabled so librsvg draws the text instead of dropping
the contents of <foreignObject>."
    (let* ((input (make-temp-file "mermaid-block-" nil ".mmd"))
           (output (make-temp-file "mermaid-block-" nil ".svg"))
           (config (make-temp-file "mermaid-config-" nil ".json"))
           (colors (markdown-mermaid--get-theme-colors))
           (bg (alist-get 'bg colors))
           (labels `((htmlLabels . ,json-false))))
      (with-temp-file config
        (insert (json-encode
                 `((theme . "base")
                   (htmlLabels . ,json-false)
                   (flowchart . ,labels)
                   (class . ,labels)
                   (state . ,labels)
                   (themeVariables . ((primaryColor . ,bg)
                                      (primaryTextColor . ,(alist-get 'fg colors))
                                      (primaryBorderColor . ,(alist-get 'secondary colors))
                                      (lineColor . ,(alist-get 'lines colors))
                                      (secondaryColor . ,bg)
                                      (tertiaryColor . ,bg)))))))
      (with-temp-file input
        (insert mermaid-code))
      (message "Compiling Mermaid block...")
      (call-process markdown-mermaid-mmdc-path nil "*mermaid-error*" nil
                    "-i" input "-o" output "-c" config "-b" "transparent")
      (if (and (file-exists-p output)
               (> (file-attribute-size (file-attributes output)) 0))
          (list output (list input output config))
        (dolist (file (list input output config))
          (when (file-exists-p file) (delete-file file)))
        nil))))

(defvar my/markdown-preview-token nil
  "Cached GitHub token for `org-markdown-preview'.")

(defun my/markdown-preview-gh-auth ()
  "Return the GitHub user and token for the Markdown API renderer."
  (cons "gilbertwong96"
        (or my/markdown-preview-token
            (setq my/markdown-preview-token
                  (string-trim (shell-command-to-string "gh auth token"))))))

(use-package ghub
  :straight t
  :defer t)

(use-package org-markdown-preview
  :straight (:host github :repo "KarimAziev/org-markdown-preview"
             :files ("*.el" "*.html"))
  :commands (org-markdown-preview-mode org-markdown-preview-browse-preview)
  :custom
  (org-markdown-preview-gh-auth #'my/markdown-preview-gh-auth)
  :ensure-system-package pandoc)

(provide 'init-markdown)
;;; init-markdown.el ends here
