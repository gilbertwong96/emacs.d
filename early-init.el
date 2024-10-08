;;; Package --- early-init.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Early init file

;;; Code:

(setq package-native-compile t)
(setq package-enable-at-startup nil)

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 128 1024 1024))
      (init-gc-cons-threshold (* 1024 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            #'(lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Hide tool bar
(tool-bar-mode -1)
;; Hide menu bar
(menu-bar-mode -1)

;; Hide scroll bar
(scroll-bar-mode -1)
;; (fringe-mode -1)

(add-to-list 'default-frame-alist '(width  . 160))
(add-to-list 'default-frame-alist '(height . 50))

;; Use straight.el as package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(provide 'early-init)
;;; early-init.el ends here
