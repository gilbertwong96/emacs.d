;;; Package ---  init-db.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Configuration for db connection

;;; Code:

;; .dir-locals.el
;; Uss .dir-locals.el to define the project-specific connection details
;; ((sql-mode . ((sql-postgres-login-params
;;                '((user :default "username")
;;                  (database :default "app_development")
;;                  (server :default "localhost")
;;                  (port :default 5432))))))

(use-package sqlformat
  :straight t
  :hook (sql-mode . sqlformat-on-save-mode))

(provide 'init-db)
;;; init-db.el ends here
