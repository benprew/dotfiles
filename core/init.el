(prelude-require-packages '(solarized-theme dash-at-point web-mode jq-mode fish-mode graphviz-dot-mode dumb-jump))

(dumb-jump-mode)

(setq projectile-project-search-path '("~/src/"))
(projectile-cleanup-known-projects)

(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

(require 'prelude-helm-everywhere)
(require 'prelude-org)
(require 'dash-at-point)

(add-hook 'prog-mode-hook 'prelude-enable-whitespace)
(add-hook 'prog-mode-hook (lambda () (smartparens-mode -1)))
(setq-default whitespace-line-column 88)
(setq-default fill-column 88)

(setq global-flycheck-mode t)
(global-flycheck-mode)

(windmove-default-keybindings 'meta)

(global-set-key (kbd "<ESC> <left>")  'windmove-left)
(global-set-key (kbd "<ESC> <right>") 'windmove-right)
(global-set-key (kbd "<ESC> <up>")    'windmove-up)
(global-set-key (kbd "<ESC> <down>")  'windmove-down)

(setq confirm-kill-emacs 'yes-or-no-p)

(setq initial-scratch-message ";Don't ignore your dreams\n;Don't work too much\n;Say what you think\n;Cultivate friendships\n;Be happy.\n\n")

(server-start)

(autoload 'jq-mode "jq-mode.el"
    "Major mode for editing jq files" t)
(add-to-list 'auto-mode-alist '("\\.jq$" . jq-mode))

(abbrev-mode -1)
(global-auto-composition-mode -1)
(smartparens-global-mode -1)
(smartparens-mode -1)
(electric-pair-mode -1)
(electric-quote-mode -1)

;; stop making files with #! executable
(remove-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; for git in /usr/local/bin
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(setq tramp-default-method "ssh")

(setq undo-tree-auto-save-history nil)
(if (display-graphic-p)
    (setq browse-url-browser-function 'browse-url-default-browser))
