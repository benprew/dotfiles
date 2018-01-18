(prelude-require-packages '(puppet-mode))

(autoload 'puppet-mode "puppet-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.pp\\'" . puppet-mode))
