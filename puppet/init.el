(require-el-get 'puppet-mode)
(require-el-get 'flymake-puppet)

(post-init (lambda()
  (setq flymake-puppet-executable "/usr/local/bin/puppet-lint")))
