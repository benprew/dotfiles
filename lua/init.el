(add-to-list 'auto-mode-alist '("\\.script\\'" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.gui_script\\'" . lua-mode))

(add-hook 'lua-mode-hook (lambda () (whitespace-mode -1)))
(add-hook 'lua-mode-hook (lambda () (setq-local indent-tabs-mode t)))

(setq-default lua-indent-level 4)
(setq-default tab-width 4)
