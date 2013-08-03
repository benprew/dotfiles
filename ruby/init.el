(require-el-get 'ruby-mode)
(require-el-get 'inf-ruby)
(require-el-get 'rinari)
(require-el-get 'rhtml-mode)
(require-el-get 'yaml-mode)
(require-el-get 'haml-mode)

(global-set-key "\M-;" 'rinari-rgrep)

(setq rinari-tags-file-name "TAGS")

(post-init (lambda()
  (autoload 'ruby-mode "ruby-mode" nil t)
  (add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
  (add-hook 'ruby-mode-hook '(lambda ()
                               (setq ruby-deep-arglist t)
                               (setq ruby-deep-indent-paren nil)
                               (setq c-tab-always-indent nil)
                               (modify-syntax-entry ?_ "w" ruby-mode-syntax-table)
                               (require 'inf-ruby)
                               (require 'ruby-compilation)
                               (define-key ruby-mode-map (kbd "M-r") 'run-rails-test-or-ruby-buffer)))
  (autoload 'rhtml-mode "rhtml-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . rhtml-mode))
  (add-to-list 'auto-mode-alist '("\\.rjs\\'" . rhtml-mode))
  (add-hook 'rhtml-mode-hook '(lambda ()
                           (modify-syntax-entry ?_ "w" rhtml-mode-syntax-table)
                           (define-key rhtml-mode-map (kbd "M-s") 'save-buffer)))
  (autoload 'yaml-mode "yaml-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
  (autoload 'css-mode "css-mode" nil t)
  (add-hook 'css-mode-hook '(lambda ()
                              (setq css-indent-level 2)
                              (setq css-indent-offset 2)))

  (autoload 'haml-mode "haml-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))

  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH"))) ;for brew-installed ruby

  (autoload 'run-ruby "inf-ruby"
    "Run an inferior Ruby process")
  (autoload 'inf-ruby-keys "inf-ruby"
    "Set local key defs for inf-ruby in ruby-mode")
  (add-hook 'ruby-mode-hook
            '(lambda ()
               (inf-ruby-keys)))

  (add-hook 'js-mode-hook '(lambda ()
                             (setq js-indent-level 2)
                             (modify-syntax-entry ?_ "w" js-mode-syntax-table)))
))
