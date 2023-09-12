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

;; If the value is greater than 100, redisplay will never recenter point, but will always
;; scroll just enough text to bring point into view, even if you move far away. A value of
;; zero means always recenter point if it moves off screen."
(setq scroll-conservatively 101)
(setq scroll-margin 10)

;; Set transparency
(defun set-transparency (val)
  "Setting transparency for emacs"
  (interactive
   (list (read-number "Transparent value: ")))
  (set-frame-parameter nil 'alpha 90)
;;  (add-to-list 'default-frame-alist '(alpha . val))
  )

(set-transparency 90)

;; Set titlebar theme
(set-frame-parameter nil 'ns-appearance 'dark)
(set-frame-parameter nil 'ns-transparent-titlebar nil)

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

(provide 'init-settings)
;;; init-settings.el ends here
