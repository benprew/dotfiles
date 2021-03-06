(setq inhibit-startup-screen t)
(setq initial-scratch-message ";Don't ignore your dreams\n;Don't work too much\n;Say what you think\n;Cultivate friendships\n;Be happy.\n\n")

(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(setq confirm-kill-emacs 'yes-or-no-p)
(setq-default fill-column 88)
(setq-default whitespace-line-column 88)
(setq custom-file "~/.emacs.d/custom.el")
(setq load-prefer-newer 't)
(setq whitespace-mode 1)
(setq prelude-whitespace 't)
(setq prelude-clean-whitespace-on-save nil)
(setq global-flycheck-mode t)

(if (display-graphic-p)
    (setq browse-url-browser-function 'browse-url-default-browser)
  (setq browse-url-browser-function 'eww-browse-url))

(require 'use-package)
(require 'prelude-helm-everywhere)

(use-package graphviz-dot-mode
  :defer 1
  :ensure t)

(use-package jq-mode
  :defer t
  :ensure t
  :mode "\\.jq\\'")

(use-package fish-mode
  :defer t
  :ensure t
  :mode ("\\.fish\\'" "\\.fish\\.symlink\\'"))

(use-package dumb-jump
  :defer 1
  :ensure t
  :config (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(defun bprew-projectile-mode-line ()
  "Customize projecile mode line."
  (if (file-remote-p default-directory)
      " Pr"
    (format " Pr[%s]" (projectile-project-name))))
(use-package projectile
  :defer 3
  :ensure t
  :config
  (setq projectile-project-search-path '("~/src/"))
  (setq projectile-mode-line-function 'bprew-projectile-mode-line)
  (projectile-cleanup-known-projects)
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package ag
  :defer t
  :ensure t)

;; https://github.com/lewang/ws-butler
(use-package ws-butler
  :defer 2
  :ensure t
  :config (ws-butler-global-mode 1))

(use-package ssh-config-mode
  :defer t
  :ensure t
  :config
  (autoload 'ssh-config-mode "ssh-config-mode" t)
  :mode ("/\\.ssh/config\\'" "/rap_ssh_configconfig\\'" "/sshd?_config\\'"
   "/knownhosts\\'" "/authorized_keys2?\\'")
  :hook turn-on-font-lock)

(use-package crontab-mode
  :ensure t
  :defer t
  :mode "\\.cron'")

;; https://github.com/k1LoW/emacs-ansible
(use-package ansible
  :ensure t
  :defer t
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
  :defer t
  :hook (yaml-mode . ansible-doc-mode))

(use-package gitconfig-mode
  :mode "gitconfig\\.symlink\\'")

(use-package emacs-lisp-mode
  :mode "\\.el\\.symlink\\'")
