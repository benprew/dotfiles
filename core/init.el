(global-set-key (kbd "C-c e n") 'flymake-goto-next-error)
(global-set-key (kbd "C-c e p") 'flymake-goto-prev-error)
(global-set-key (kbd "C-;") 'comment-dwim)

;; automatically save buffers when focus is lost
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

;; Emacs defauls behave more like VS-Code defaults
;; Kill current buffer (instead of asking first buffer name)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(delete-selection-mode 1)

;; Open recent files
(global-set-key (kbd "C-c r") 'recentf-open-files)

;; Enable auto-pair globally
(electric-pair-mode 1)

(setq initial-scratch-message ";Don't ignore your dreams\n;Don't work too much\n;Say what you think\n;Cultivate friendships\n;Be happy.\n\n")

(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(setq confirm-kill-emacs 'y-or-n-p)
(setq-default fill-column 82)
(setq-default whitespace-line-column 82)
(setq custom-file "~/.emacs.d/custom.el")
(setq load-prefer-newer 't)
(setq whitespace-mode 1)
(setq column-number-mode t)
(setq global-flycheck-mode 't)
(global-auto-revert-mode t)

;; save backup files into a single directory
(make-directory "~/.emacs_backups/" t)
(make-directory "~/.emacs_autosave/" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs_autosave/" t)))
(setq backup-directory-alist '(("." . "~/.emacs_backups/")))
(setq create-lockfiles nil)

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

(add-to-list 'auto-mode-alist '("gitconfig\\.symlink\\'" . gitconfig-mode))
(add-to-list 'auto-mode-alist '("\\.el\\.symlink\\'" . emacs-lisp-mode))

(use-package aidermacs
  :straight (:host github :repo "MatthewZMD/aidermacs" :files ("*.el"))
  :config
  (setq aidermacs-args '("--model" "anthropic/claude-3-5-sonnet-20241022"))
  (setenv "ANTHROPIC_API_KEY"
          (with-temp-buffer
            (insert-file-contents (expand-file-name "~/secrets/claude.ai.key"))
            (string-trim (buffer-string))))

<<<<<<< Updated upstream
  (global-set-key (kbd "C-c a") 'aidermacs-transient-menu))

;; Change multiline input key (default is S-<return>)
(setq aidermacs-comint-multiline-newline-key "C-<return>")
=======
;; (use-package gitconfig-mode
;;   :mode "gitconfig\\.symlink\\'")

;; (use-package emacs-lisp-mode
;;   :mode "\\.el\\.symlink\\'")
>>>>>>> Stashed changes

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

;; zeal is like dash
(use-package zeal-at-point
  :ensure t)
(global-set-key "\C-cd" 'zeal-at-point)

;; Add magit to project.el selection
(with-eval-after-load 'project
  (define-key project-prefix-map "m" #'magit-project-status)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))

;; Improved search function (use f2) in isearch to show
;; From http://yummymelon.com/devnull/improving-emacs-isearch-usability-with-transient.html
(require 'transient)
(transient-define-prefix cc/isearch-menu ()
  "isearch Menu"
  [["Edit Search String"
    ("e"
     "Edit the search string (recursive)"
     isearch-edit-string
     :transient nil)
    ("w"
     "Pull next word or character word from buffer"
     isearch-yank-word-or-char
     :transient nil)
    ("s"
     "Pull next symbol or character from buffer"
     isearch-yank-symbol-or-char
     :transient nil)
    ("l"
     "Pull rest of line from buffer"
     isearch-yank-line
     :transient nil)
    ("y"
     "Pull string from kill ring"
     isearch-yank-kill
     :transient nil)
    ("t"
     "Pull thing from buffer"
     isearch-forward-thing-at-point
     :transient nil)]

   ["Replace"
    ("q"
     "Start ‘query-replace’"
     isearch-query-replace
     :if-nil buffer-read-only
     :transient nil)
    ("x"
     "Start ‘query-replace-regexp’"
     isearch-query-replace-regexp
     :if-nil buffer-read-only
     :transient nil)]]

  [["Toggle"
    ("X"
     "Toggle regexp searching"
     isearch-toggle-regexp
     :transient nil)
    ("S"
     "Toggle symbol searching"
     isearch-toggle-symbol
     :transient nil)
    ("W"
     "Toggle word searching"
     isearch-toggle-word
     :transient nil)
    ("F"
     "Toggle case fold"
     isearch-toggle-case-fold
     :transient nil)
    ("L"
     "Toggle lax whitespace"
     isearch-toggle-lax-whitespace
     :transient nil)]

   ["Misc"
    ("o"
     "occur"
     isearch-occur
     :transient nil)]])

(define-key isearch-mode-map (kbd "<f2>") 'cc/isearch-menu)

;; End improved search
