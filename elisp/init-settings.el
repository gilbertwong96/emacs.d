;;; Package --- init-settings.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic settings for Gilbert's Emacs

;;; Code:

;; Initialize ENV variables
(setenv "EDITOR" "emacsclient")

;; Start Server
(server-start)

(defun gilbert/vim-like-scrolloff (scrolloff)
  "Make the Emacs scroll like vim.
If the SCROLLOFF is greater than 100, redisplay will never recenter point, but
will always scroll just enough text to bring point into view, even if you
move far away A value of zero means always recenter point if it moves off
screen."
  (setq scroll-conservatively 101)
  (setq scroll-margin scrolloff)
  )

(defun gilbert/setting-pixel-scroll ()
  "Setting pixel scroll settings."
  ;; Pixel scroll
  (pixel-scroll-precision-mode)

  ;; When scrolling rapidly, skip precise, on-the-fly fontification and layout
  ;; so scrolling feels smoother; may briefly show unfontified text or slightly
  ;; inaccurate positions with mixed fonts or variable line heights.
  (customize-set-variable 'fast-but-imprecise-scrolling t)

  ;; Defer JIT fontification until Emacs has been idle for ~0.1s to reduce
  ;; work during scrolling; expect occasional briefly unfontified regions
  ;; that get filled in once input pauses.
  (customize-set-variable 'jit-lock-defer-time 0.05)

  ;; point always keeps its screen position
  (customize-set-variable 'scroll-preserve-screen-position 'always)

  ;; Enable horizontal scrolling by tilting mouse wheel or via touchpad.
  (customize-set-variable 'mouse-wheel-tilt-scroll t)

  ;; Swap direction of ‘wheel-right’ and ‘wheel-left’
  (customize-set-variable 'mouse-wheel-flip-direction t)

  ;; The line displaying point in each window is automatically scrolled horizontally
  ;; to make point visible
  (customize-set-variable 'auto-hscroll-mode 1)
  )

;; Set transparency
(defun gilbert/set-transparency (value)
  "Setting transparency VALUE for Emacs."
  (interactive
   (list (read-number "Transparent value: ")))
  (set-frame-parameter nil 'alpha value)
  )

(defun gilbert/set-font ()
  "Set gilbert's favourite fonts."

  (set-face-attribute 'default nil :height 130)
  (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font Mono")
  (cond ((eq system-type 'darwin)
         ;; (set-face-attribute 'default nil :family "Fira Code")
         ;; (set-face-attribute 'default nil :family "ComicShannsMono Nerd Font Mono")

         ;; default font size (point * 10)
         ;;
         ;; WARNING!  Depending on the default font,
         ;; if the size is not supported very well, the frame will be clipped
         ;; so that the beginning of the buffer may not be visible correctly.

         ;; use specific font for CJK charset.
         ;; if you want to use different font size for specific charset,
         ;; add :size POINT-SIZE in the font-spec.
         (set-fontset-font t 'han "PingFang SC"))
        ((eq system-type 'gnu/linux)
         (set-face-attribute 'variable-pitch nil :family "IosevkaTermSlab Nerd Font Mono")
         (set-fontset-font t 'han "Noto Sans CJK"))
        )
  )


(defun gilbert/set-config-system-utf8 ()
  "Set encoding to utf8."
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
  (prefer-coding-system 'utf-8))

(defun gilbert/clean-temp-files ()
  "Use no-littering to clean temp files."
  (use-package no-littering
    :straight t)
  ;; Disable generating annoyed backup files
  (setq make-backup-files nil)
  )

(defun gilbert/fullscreen ()
  "Make sure Emacs init with fullscreen."
  (add-hook 'emacs-startup-hook 'toggle-frame-fullscreen)
  )

(defun gilbert/set-column-indicator (column)
  "Set column indicator for Emacs, COLUMN shoule be integer."
  (customize-set-variable 'display-fill-column-indicator-column column)
  (dolist (hook (list 'prog-mode-hook 'text-mode-hook))
    (add-hook hook #'(lambda () (display-fill-column-indicator-mode)))))

(defun gilbert/show-trailing-whitespace ()
  "Show trailing spaces."
  (dolist (hook (list 'prog-mode-hook 'text-mode-hook))
    (add-hook hook #'(lambda () (setq-local show-trailing-whitespace t)))))


(defun gilbert/better-defaults ()
  "Better defaults setting for Emacs."

  ;; Do not load startup screen
  (setq inhibit-startup-screen t)

  ;; Prevent insert tabs
  (setq-default indent-tabs-mode nil)

  ;; Enable hs-minor-mode for program files
  ;; (add-hook 'prog-mode-hook #'hs-minor-mode)

  ;; Make sure the tab width is 4 characters
  (setq-default tab-width 4)

  ;; Disable ring bell
  (setq ring-bell-function 'ignore)

  ;; Enable auto-revert mode
  (add-hook 'after-init-hook 'global-auto-revert-mode)

  ;; Enable display-line-numbers-mode for program files
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)

  ;; Highlight current line
  (add-hook 'after-init-hook 'global-hl-line-mode)


  ;; Enable file auto save
  (setq auto-save-visited-mode t)

  ;; Enable column mode
  (column-number-mode t)

  (gilbert/setting-pixel-scroll)

  ;; Set transparency 90
  (gilbert/set-transparency 100)

  (gilbert/set-config-system-utf8)

  ;; (gilbert/vim-like-scrolloff 12)

  (gilbert/set-column-indicator 100)

  (gilbert/set-font)

  (gilbert/set-config-system-utf8)

  (gilbert/show-trailing-whitespace)
  ;; (customize-set-variable 'tramp-save-ad-hoc-proxies t)

  (gilbert/clean-temp-files)

  (gilbert/fullscreen)
  )

(gilbert/better-defaults)

(use-package doom-modeline
  :straight t
  :config
  (doom-modeline-mode 1)
  (setq inhibit-compacting-font-caches t))

(use-package ligature
  :straight t
  :config
  ;; Enable the www ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))

  ;; Enable ligatures in programming modes
  (ligature-set-ligatures '(erlang-mode elixir-ts-mode heex-ts-mode)
                          '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                            ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                            "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                            "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                            "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                            "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                            "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                            "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                            "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                            "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

  (global-ligature-mode 't))

(use-package iedit :straight t)

(use-package recentf)

(use-package uniquify)

(use-package page-break-lines
  :straight t
  :config
  (global-page-break-lines-mode)
  )

;; Toggle auto insert of closing bracket

;; auto close bracket insertion.
(use-package electric
  :hook
  (prog-mode . electric-pair-mode)
  :custom
  (electric-pair-pairs '((?\{ . ?\})))
  )


(use-package hl-todo
  :straight t
  :hook
  (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
      '(("TODO"   . "#FF0000")
        ("FIXME"  . "#FF0000")
        ("NOTE"   . "#FFFFFF")
        ("WARN"   . "#FF4500")
        ("DEBUG"  . "#A020F0")
        ("GOTCHA" . "#FF4500")
        ("STUB"   . "#1E90FF")))
  )

;; Rainbow delimiters makes nested delimiters easier to understand
(use-package rainbow-delimiters
  :straight t
  :hook ((prog-mode . rainbow-delimiters-mode)))

;; PO/POT file header customization
(with-eval-after-load 'po-mode
  (setopt po-default-file-header "")
  (setopt po-default-copyright-holder "Gilbert Wong")
  (setopt po-default-author "Gilbert Wong")
  (setopt po-default-email "gilbertwong96@icloud.com")
  (setopt po-default-year (format-time-string "%Y")))

(provide 'init-settings)
;;; init-settings.el ends here
