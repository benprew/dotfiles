(require-el-get 'magit)

(post-init (lambda ()
             (global-set-key "\C-cs" 'magit-status)))
