;;; init-erlang.el --- Support for C/C++  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'ccls)
  (gilbert/add-hook '(c-mode c++-mode objc-mode cuda-mode)
                    '((lambda () (require 'ccls) (lsp)
                        ))
                    )
  )

(provide 'init-c++)
;;; init-c++.el ends here
