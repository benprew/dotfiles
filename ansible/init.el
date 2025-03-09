;; https://github.com/k1LoW/emacs-ansible
(use-package ansible
  :ensure t
  :defer 3
  :hook
  (ansible . ansible-auto-decrypt-encrypt)
  (yaml-mode . ansible)
  :config
  ;;(global-set-key (kbd "C-c b") 'ansible-decrypt-buffer)
  ;;(global-set-key (kbd "C-c g") 'ansible-encrypt-buffer)
  ;;(add-hook 'ansible-hook 'ansible-auto-decrypt-encrypt)
  (setq ansible-vault-password-file "~/.ansible-vault-password"))

;; https://github.com/emacsorphanage/ansible-doc
(use-package ansible-doc
  :ensure t
  :defer 3
  :hook (yaml-mode . ansible-doc-mode))
