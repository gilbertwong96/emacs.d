;; Package --- init-web-*- lexical-binding: t; -*-

;;; Commentary:

;; For Web Development

;;; Code:

(use-package flycheck :straight t :defer t)

(use-package nginx-mode
  :straight t
  :defer t
  :commands nginx-mode)

(use-package rainbow-mode
  :straight t
  :defer t
  :mode
  ("\\.css\\'" . css-ts-mode)
  :hook
  (css-ts-mode . rainbow-mode)
  (js-ts-mode . rainbow-mode)
  (typescript-ts-mode . rainbow-mode))

(use-package js2-mode
  :straight t
  :defer t
  :mode
  ("\\.js\\'" . js-ts-mode)
  ("\\.json\\'" . json-ts-mode)
  ("\\.ts\\'" . typescript-ts-mode)
  :config
  ;; Hook it in for shell scripts running via node.js
  (add-to-list 'interpreter-mode-alist '("node" . js-ts-mode))
  )

(use-package skewer-mode
  :straight t
  :after js2-mode
  :defer t
  :hook
  (js2-mode  . skewer-mode)
  (css-mode  . skewer-css-mode)
  (html-mode . skewer-html-mode))

(use-package js2-refactor
  :straight t
  :after js2-mode
  :defer t
  :hook
  (js2-mode . js2-refactor-mode))

(use-package po-mode
  :straight t
  :defer t
  :mode
  ("\\.pot?\\'" . po-mode))

(use-package emmet-mode
  :straight t
  :defer t
  :hook
  (web-mode . emmet-mode)
  (heex-ts-mode . emmet-mode)
  )

(provide 'init-web)
;;; init-web.el ends here
