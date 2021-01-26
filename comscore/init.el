(prelude-require-packages '(crontab-mode ansible ansible-doc ansible-vault))

;; https://github.com/k1LoW/emacs-ansible
(add-hook 'yaml-mode-hook '(lambda () (ansible 1)))

;; https://github.com/emacsorphanage/ansible-doc
(add-hook 'yaml-mode-hook #'ansible-doc-mode)

(add-to-list 'auto-mode-alist '("\\.cron$" . crontab-mode))
