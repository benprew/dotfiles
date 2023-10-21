(straight-use-package
 '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))
(require 'nano-defaults)
(require 'nano-session)
(require 'nano-bindings)

(global-set-key (kbd "C-c e n") 'flymake-goto-next-error)
(global-set-key (kbd "C-c e p") 'flymake-goto-prev-error)


(setq initial-scratch-message ";Don't ignore your dreams\n;Don't work too much\n;Say what you think\n;Cultivate friendships\n;Be happy.\n\n")

(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(setq confirm-kill-emacs 'y-or-n-p)
(setq-default fill-column 82)
(setq-default whitespace-line-column 82)
(setq load-prefer-newer 't)
(setq whitespace-mode 1)
(setq column-number-mode t)
(setq global-flycheck-mode 't)
(global-auto-revert-mode t)

;;;; Emacs Startup Performance
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; this stops tramp from making an ssh connection to autosave
;; this doesn't seem to actually work...
;; (add-to-list 'backup-directory-alist
;;              (cons "." "~/.emacs.d/tramp-autosave/"))
;; (customize-set-variable
;;  'tramp-backup-directory-alist backup-directory-alist)

;; (if (display-graphic-p)
;;     (setq browse-url-browser-function 'browse-url-default-browser)
;;   (setq browse-url-browser-function 'eww-browse-url))

(use-package magit
  :defer 3
  :ensure t)

(use-package graphviz-dot-mode
  :defer 3
  :ensure t)

(use-package jq-mode
  :defer 3
  :ensure t
  :mode "\\.jq\\'")

(use-package dumb-jump
  :defer 1
  :ensure t
  :config (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


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
  :mode ("/\\.ssh/config\\'" "/rap_ssh_config\\'" "/sshd?_config\\'"
   "/knownhosts\\'" "/authorized_keys2?\\'" "_ssh_config\\'")
  :hook turn-on-font-lock)

(use-package crontab-mode
  :ensure t
  :defer t
  :mode "\\.cron\\'")

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

(use-package gitconfig-mode
  :mode "gitconfig\\.symlink\\'")

(use-package emacs-lisp-mode
  :mode "\\.el\\.symlink\\'")

(use-package helpful
  :ensure t
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function))

(use-package eglot
  :ensure t
  :defer 2
  :hook (python-mode . eglot-ensure)(go-mode . eglot-ensure))

;; optional if you want which-key integration
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package zeal-at-point
  :ensure t)


