;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))

(use-package graphviz-dot-mode
  :ensure t)

(use-package jq-mode
  :ensure t
  :mode "\\.jq'")

(use-package fish-mode
  :ensure t
  :mode "\\.fish'")

(use-package dumb-jump
  :ensure t
  :init (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


(setq projectile-project-search-path '("~/src/"))
(projectile-cleanup-known-projects)

(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

(require 'prelude-helm-everywhere)

(add-hook 'prog-mode-hook 'prelude-enable-whitespace)
(add-hook 'prog-mode-hook (lambda () (smartparens-mode -1)))
(setq-default whitespace-line-column 88)
(setq-default fill-column 88)

(setq global-flycheck-mode t)

(windmove-default-keybindings 'shift)

(setq confirm-kill-emacs 'yes-or-no-p)

(setq initial-scratch-message ";Don't ignore your dreams\n;Don't work too much\n;Say what you think\n;Cultivate friendships\n;Be happy.\n\n")

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
