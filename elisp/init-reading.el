;;; Package --- init-reading.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Enhance reading experience

;;; Code:

(use-package pdf-tools
  :straight t
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install)
  )

(use-package nov
  :straight t
  :mode ("\\.epub\\'" . nov-mode)
  :defer t
  :config
  (setq nov-unzip-program (executable-find "bsdtar"))
  (setq nov-unzip-args '("-xC" directory "-f" filename)))

(provide 'init-reading)
;;; init-reading.el ends here
