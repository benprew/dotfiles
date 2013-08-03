(require-el-get 'puppet-mode)
(require-el-get '(:name flymake-puppet
                        :type elpa))
(require-el-get '(:name flymake-easy
                        :type elpa))

(post-init (lambda ()
             (autoload 'puppet-mode "puppet-mode" nil t)
             (add-to-list 'auto-mode-alist '("\\.pp\\'" . puppet-mode))
             (autoload 'flymake-easy "flymake-easy" nil t)
             (autoload 'flymake-puppet "flymake-puppet" nil t)
             (add-hook 'puppet-mode-hook 'flymake-puppet-load)
             (setq flymake-puppet-executable "/usr/local/bin/puppet-lint")))


