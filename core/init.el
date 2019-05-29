(prelude-require-packages '(solarized-theme dash-at-point web-mode helm-w3m jq-mode fish-mode graphviz-dot-mode dumb-jump))


(dumb-jump-mode)

(setq projectile-project-search-path '("~/src/"))
(projectile-cleanup-known-projects)

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(setq w3m-use-cookies t)
;; optional keyboard short-cut
;;(global-set-key "\C-xm" 'browse-url-at-point)

(require 'prelude-helm-everywhere)
(require 'prelude-org)
(require 'dash-at-point)

(add-hook 'prog-mode-hook 'prelude-enable-whitespace)
(add-hook 'prog-mode-hook (lambda () (smartparens-mode -1)))

(setq global-flycheck-mode t)
(global-flycheck-mode)

(windmove-default-keybindings 'super)

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
