;; this breaks when I'm editing files in other people's directories on linliveanalytics1
(if (<= emacs-major-version 25)
    (setq undo-tree-auto-save-history nil))

(require 'use-package)

(setq ansible-vault-password-file "/home/ben/.ansible-vault-password-as")
