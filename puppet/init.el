(require-el-get 'puppet-mode)

(post-init (lambda ()
             (autoload 'puppet-mode "puppet-mode" nil t)
             (add-to-list 'auto-mode-alist '("\\.pp\\'" . puppet-mode))))
