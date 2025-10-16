;;; Package --- init-theme.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Customize Gilbert's Emacs theme

;;; Code:

(use-package all-the-icons
  :straight t
  :defer t)

(use-package doom-themes
  :straight t
  :init
  ;; Set titlebar theme
  (set-frame-parameter nil 'ns-appearance 'dark)
  (set-frame-parameter nil 'ns-transparent-titlebar nil)

  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-monokai-classic t)
  ;; (load-theme 'doom-gruvbox-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package tab-bar
  :custom
  ;; Hide tab bar if only one tab exists
  (tab-bar-show 1)
  ;; Hide the tab close/X button
  (tab-bar-close-button-show nil)
  ;; Hide the "+" button
  (tab-bar-new-button-show t)
  ;; Show tab numbers
  (tab-bar-tab-hints t)
  ;; Configure tab bar format
  (tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  )

(use-package breadcrumb
  :straight t
  :config
  (breadcrumb-mode))

(provide 'init-theme)
;;; init-theme.el ends here
