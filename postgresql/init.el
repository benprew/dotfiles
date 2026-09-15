;;; -*- lexical-binding: t; -*-

(use-package sqlformat
  :ensure t
  :commands (sqlformat sqlformat-buffer sqlformat-region)
  :custom
  (sqlformat-command 'pgformatter)
  (sqlformat-args '("-g")))
