;;; Package --- init-telegram.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Integrate telega into Emacs

;;; Code:

;; Telega
(use-package telega
  :if (eq system-type 'darwin)
  :straight t
  :defer t
  ;; :hook
  ;; (telega-load . global-telega-mnz-mode)
  :config
  :custom
  (telega-server-libs-prefix "/opt/homebrew/Cellar/tdlib/HEAD-9b6ff58")
  )

(use-package language-detection
  :straight t
  :defer t
  )

(use-package telega-mnz
  :after (telega language-detection)
  :custom
  (telega-mnz-use-language-detection 'on)
  :hook
  (telega-load . global-telega-mnz-mode)
  )


(provide 'init-telegram)
;;; init-telegram.el ends here
