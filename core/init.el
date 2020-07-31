(prelude-require-packages '(web-mode jq-mode fish-mode graphviz-dot-mode dumb-jump helm-ag))

;; dump-jump now integrates with xref-backend, so you can use M-. and M-, to search and return
(dumb-jump-mode)
(add-to-list 'xref-backend-functions #'dumb-jump-xref-activate)

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

(windmove-default-keybindings 'meta)

(global-set-key (kbd "<ESC> <left>")  'windmove-left)
(global-set-key (kbd "<ESC> <right>") 'windmove-right)
(global-set-key (kbd "<ESC> <up>")    'windmove-up)
(global-set-key (kbd "<ESC> <down>")  'windmove-down)

(setq confirm-kill-emacs 'yes-or-no-p)

(setq initial-scratch-message ";Don't ignore your dreams\n;Don't work too much\n;Say what you think\n;Cultivate friendships\n;Be happy.\n\n")

(autoload 'jq-mode "jq-mode.el"
    "Major mode for editing jq files" t)
(add-to-list 'auto-mode-alist '("\\.jq$" . jq-mode))

(abbrev-mode -1)
(global-auto-composition-mode -1)
(smartparens-global-mode -1)
(smartparens-mode -1)
(electric-pair-mode -1)
(electric-quote-mode -1)

(setq undo-tree-auto-save-history nil)
(if (display-graphic-p)
    (setq browse-url-browser-function 'browse-url-default-browser))


(server-start)
