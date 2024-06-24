;; Package --- init-web-*- lexical-binding: t; -*-

;;; Commentary:

;; For Web Development

;;; Code:

(use-package flycheck :straight t)

(use-package js2-mode
  :straight t
  :config
  ;; Make js2-mode as major mode for JavaScript editing
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  ;; Hook it in for shell scripts running via node.js
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
  )

(use-package skewer-mode
  :straight t
  :after js2-mode
  :hook
  (js2-mode  . skewer-mode)
  (css-mode  . skewer-css-mode)
  (html-mode . skewer-html-mode))

(use-package js2-refactor
  :straight t
  :after js2-mode
  :hook
  (js2-mode . js2-refactor-mode))

(use-package tide
  :straight t
  :after (flycheck js2-mode)
  :hook
  ((typescript-ts-mode . tide-setup)
   (tsx-ts-mode . tide-setup)
   (typescript-ts-mode . tide-hl-identifier-mode)
   (before-save . tide-format-before-save)
   )
  )

(provide 'init-web)
;;; init-web.el ends here
