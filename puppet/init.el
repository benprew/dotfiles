(require-el-get 'puppet-mode)
(require-el-get '(:name flymake-puppet
                        :description "Emacs flymake integration for puppet-lint"
                        :type git
                        :url "https://github.com/grimradical/puppet-flymake"
                        :after (progn
                                 (add-hook 'puppet-mode-hook 'flymake-puppet-load))))
