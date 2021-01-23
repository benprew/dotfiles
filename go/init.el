(prelude-require-packages '(go-mode go-playground go-projectile))

(require 'prelude-go)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook (lambda () (whitespace-mode -1)))

(add-hook 'before-save-hook 'gofmt-before-save)
(add-to-list 'interpreter-mode-alist
             '("gorun" . go-mode))
