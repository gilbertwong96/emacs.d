;;; Package --- init-flutter.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Enhance Flutter Development

;;; Code:

(use-package groovy-mode
  :straight t
  :defer t)

(use-package gradle-mode
  :straight t
  :defer t)

(use-package dart-mode :straight t)

(use-package flutter
  :straight t
  :after dart-mode
  :init
  (local-leader-def
    "f" '(:ignore t :which-key "flutter")
    "fr" '(flutter-run-or-hot-reload :which-key "Run or hot reload if ready")
    "ft" '(flutter-test-all :which-key "Execute flutter test")
    "fd" '(flutter-run-device :which-key "Start flutter-run with DEVICE-ID")
    "fq" '(flutter-quit :which-key "Start flutter-run with DEVICE-ID")))

(provide 'init-flutter)
;;; init-flutter.el ends here
