;;; notes --- Note taking in emacs, with deft and sparkleshare
;;; Commentary:
;;;   deft is (http://jblevins.org/projects/deft/) and
;;;   sparkleshare is (http://sparkleshare.org/)

(require-el-get 'deft)

(post-init (lambda()
  (setq deft-extension "md")
  (setq deft-directory "~/notes")
  (setq deft-text-mode 'markdown-mode)
  (setq deft-use-filename-as-title nil)

  (global-set-key "\C-cn" 'deft)
))
