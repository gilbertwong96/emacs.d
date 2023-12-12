;;; init-cc.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Gilbert's Emacs Configuration

;;; Code:

(add-hook 'c-mode 'eglot-ensure)
(add-hook 'c++-mode 'eglot-ensure)

(provide 'init-cc)
;;; init-cc.el ends here
