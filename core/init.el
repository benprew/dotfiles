(prelude-require-packages '(solarized-theme dash-at-point web-mode helm-w3m jq-mode fish-mode graphviz-dot-mode))

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; optional keyboard short-cut
;;(global-set-key "\C-xm" 'browse-url-at-point)
(setq w3m-use-cookies t)

(require 'prelude-helm-everywhere)
(require 'prelude-org)
(require 'dash-at-point)

(add-hook 'prog-mode-hook 'prelude-enable-whitespace)

(setq global-flycheck-mode t)

(windmove-default-keybindings 'super)

(setq initial-scratch-message ";Don't ignore your dreams\n;Don't work too much\n;Say what you think\n;Cultivate friendships\n;Be happy.\n\n")

(server-start)

(autoload 'jq-mode "jq-mode.el"
    "Major mode for editing jq files" t)
(add-to-list 'auto-mode-alist '("\\.jq$" . jq-mode))
