;; Package --- init-web-*- lexical-binding: t; -*-

;;; Commentary:

;; For Web Development

;;; Code:

(use-package flycheck :straight t)

(use-package js2-mode
  :straight t
  :mode
  ("\\.js\\'" . js-ts-mode)
  ("\\.json\\'" . json-ts-mode)
  ("\\.ts\\'" . typescript-ts-mode)
  :config
  ;; Make js2-mode as major mode for JavaScript editing
  ;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
  ;; Make typescript-server-mode as major mode for JavaScript editing
  ;; (add-to-list 'auto-mode-alist '("\\.js\\'" . typescript-ts-mode))
  ;; Hook it in for shell scripts running via node.js
  (add-to-list 'interpreter-mode-alist '("node" . js-ts-mode))
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

(provide 'init-web)
;;; init-web.el ends here
