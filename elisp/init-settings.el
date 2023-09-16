;;; init-settings.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic settings for Gilbert's Emacs

;;; Code:


;; Do not load startup screen
(setq inhibit-startup-screen t)

;; Prevent insert tabs
(setq-default indent-tabs-mode nil)

;; Make sure the tab width is 4 characters
(setq-default tab-width 4)

;; Disable ring bell
(setq ring-bell-function 'ignore)

;; Enable auto-revert mode
(add-hook 'after-init-hook 'global-auto-revert-mode)

;; Highlight current line
(add-hook 'after-init-hook 'global-hl-line-mode)

;; Disable making backup files
(setq make-backup-files nil)

;; Enable column mode
(column-number-mode t)

;; If the value is greater than 100, redisplay will never recenter point, but will always
;; scroll just enough text to bring point into view, even if you move far away. A value of
;; zero means always recenter point if it moves off screen."
;; (setq scroll-conservatively 101)
;; (setq scroll-margin 10)

;; Set transparency
(defun set-transparency (val)
  "Setting transparency for emacs"
  (interactive
   (list (read-number "Transparent value: ")))
  (set-frame-parameter nil 'alpha 90)
;;  (add-to-list 'default-frame-alist '(alpha . val))
  )

(set-transparency 90)

(when (eq system-type 'darwin)

      ;; default Latin font (e.g. Consolas)
      (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font")

      ;; default font size (point * 10)
      ;;
      ;; WARNING!  Depending on the default font,
      ;; if the size is not supported very well, the frame will be clipped
      ;; so that the beginning of the buffer may not be visible correctly.
      (set-face-attribute 'default nil :height 125)

      ;; use specific font for CJK charset.
      ;; if you want to use different font size for specific charset,
      ;; add :size POINT-SIZE in the font-spec.
      (set-fontset-font t 'han "PingFang SC")

      ;; you may want to add different for other charset in this way.
      )

(use-package doom-modeline
  :straight t
  :ensure t
  :config
  (doom-modeline-mode 1)
  (setq inhibit-compacting-font-caches t))

;; Set encoding to utf8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8-unix)
(set-file-name-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-next-selection-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Enable display-line-numbers-mode for program files
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Show trailing spaces
(dolist (hook (list
               'prog-mode-hook
               'text-mode-hook))
  (add-hook hook #'(lambda () (setq-local show-trailing-whitespace t))))


(use-package recentf
  ;; Loads after 1 second of idle time.
  :defer 1)

(use-package uniquify
  ;; Less important than recentf.
  :defer 2)

(use-package page-break-lines
  :straight t
  :config
  (global-page-break-lines-mode)
  )

;; Toggle auto insert of closing bracket

;; auto close bracket insertion.
(add-hook 'prog-mode-hook #'electric-pair-mode)
(setq electric-pair-pairs '((?\{ . ?\})))

(customize-set-variable 'tramp-save-ad-hoc-proxies t)


(use-package good-scroll
  :straight t
  :ensure t
  :config
  (good-scroll-mode 1)
  )

;; Better smooth
(provide 'init-settings)
;;; init-settings.el ends here
