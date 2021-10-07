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
(setq prelude-clean-whitespace-on-save 1)
(setq global-flycheck-mode 'f)

;; this stops tramp from making an ssh connection to autosave
;; this doesn't seem to actually work...
;; (add-to-list 'backup-directory-alist
;;              (cons "." "~/.emacs.d/tramp-autosave/"))
;; (customize-set-variable
;;  'tramp-backup-directory-alist backup-directory-alist)

;; (if (display-graphic-p)
;;     (setq browse-url-browser-function 'browse-url-default-browser)
;;   (setq browse-url-browser-function 'eww-browse-url))

(require 'use-package)
(require 'prelude-helm-everywhere)

(use-package graphviz-dot-mode
  :defer 3
  :ensure t)

(use-package jq-mode
  :defer 3
  :ensure t
  :mode "\\.jq\\'")

(use-package fish-mode
  :defer 3
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
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c m") 'recompile))

(use-package ag
  :defer 3
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

;; (use-package eglot
;;   :ensure t
;;   :defer 2
;;   :hook (python-mode . eglot-ensure))


;;; LSP MODE

(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (ruby-mode . lsp)
         (python-mode . lsp)
         (sh-mode . lsp)
         (go-mode . lsp)
         (web-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; if you are helm user
(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

;; optional if you want which-key integration
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
