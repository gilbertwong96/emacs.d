;;; Package --- init-apple.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Apple ecosystem development utility

;;; Code:


(use-package swift-mode
  :if (eq system-type 'darwin)
  :straight t
  :mode "\\.swift\\'"
  :interpreter "swift"
  :init
  (local-leader-def
    :keymaps 'mix-minor-mode-map
    "x"  '(:ignore t :which-key "XCode")
    "xo" '(open-in-xcode :which-key "Open In XCode")
    "xb" '(xcode-build :which-key "Open In XCode")
    )
)

;; Custom function to open current file in Xcode
;;;###autoload
(defun open-in-xcode ()
  "Open current file in Xcode."
  (interactive)
  (shell-command (format "open -a Xcode %s"
                         (shell-quote-argument (buffer-file-name)))))

;; Function to build using xcodebuild
;;;###autoload
(defun xcode-build ()
  "Build the Xcode project in the current directory."
  (interactive)
  (compile "xcodebuild -configuration Debug"))

(provide 'init-apple)
;;; init-apple.el ends here
