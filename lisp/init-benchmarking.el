;;; init-benchmarking.el --- Measure startup and require times -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;;; copied from https://github.com/purcell/emacs.d/blob/master/lisp/init-benchmarking.el

(defun gilbert/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))


(defvar gilbert/require-times nil
  "A list of (FEATURE LOAD-START-TIME LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defun gilbert/require-times-wrapper (orig feature &rest args)
  "Note in `gilbert/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        (apply orig feature args)
      (when (and (not already-loaded) (memq feature features))
        (let ((time (gilbert/time-subtract-millis (current-time) require-start-time)))
          (add-to-list 'gilbert/require-times
                       (list feature require-start-time time)
                       t))))))

(advice-add 'require :around 'gilbert/require-times-wrapper)


(define-derived-mode gilbert/require-times-mode tabulated-list-mode "Require-Times"
  "Show times taken to `require' packages."
  (setq tabulated-list-format
        [("Start time (ms)" 20 gilbert/require-times-sort-by-start-time-pred)
         ("Feature" 30 t)
         ("Time (ms)" 12 gilbert/require-times-sort-by-load-time-pred)])
  (setq tabulated-list-sort-key (cons "Start time (ms)" nil))
  ;; (setq tabulated-list-padding 2)
  (setq tabulated-list-entries #'gilbert/require-times-tabulated-list-entries)
  (tabulated-list-init-header)
  (when (fboundp 'tablist-minor-mode)
    (tablist-minor-mode)))

(defun gilbert/require-times-sort-by-start-time-pred (entry1 entry2)
  (< (string-to-number (elt (nth 1 entry1) 0))
     (string-to-number (elt (nth 1 entry2) 0))))

(defun gilbert/require-times-sort-by-load-time-pred (entry1 entry2)
  (> (string-to-number (elt (nth 1 entry1) 2))
     (string-to-number (elt (nth 1 entry2) 2))))

(defun gilbert/require-times-tabulated-list-entries ()
  (cl-loop for (feature start-time millis) in gilbert/require-times
           with order = 0
           do (incf order)
           collect (list order
                         (vector
                          (format "%.3f" (gilbert/time-subtract-millis start-time before-init-time))
                          (symbol-name feature)
                          (format "%.3f" millis)))))

(defun gilbert/require-times ()
  "Show a tabular view of how long various libraries took to load."
  (interactive)
  (with-current-buffer (get-buffer-create "*Require Times*")
    (gilbert/require-times-mode)
    (tabulated-list-revert)
    (display-buffer (current-buffer))))




(defun gilbert/show-init-time ()
  (message "init completed in %.2fms"
           (gilbert/time-subtract-millis after-init-time before-init-time)))

(add-hook 'after-init-hook 'gilbert/show-init-time)


(provide 'init-benchmarking)
;;; init-benchmarking.el ends here
