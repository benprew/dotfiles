(require-el-get 'markdown-mode)

(add-hook 'post-init-hook (lambda ()
  (setq auto-mode-list
        (cons '("\\.md" . markdown-mode) auto-mode-alist))))