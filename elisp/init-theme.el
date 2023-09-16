;;; init-theme.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Customize Gilbert's Emacs theme

;;; Code:

(use-package all-the-icons
  :straight t
  :ensure t)

(use-package doom-themes
  :straight t
  :ensure t
  :init
  ;; Set titlebar theme
  (set-frame-parameter nil 'ns-appearance 'dark)
  (set-frame-parameter nil 'ns-transparent-titlebar nil)

  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-monokai-pro t)


  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(provide 'init-theme)
;;; init-theme.el ends here
