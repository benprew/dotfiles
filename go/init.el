(prelude-require-packages '(go-mode go-playground go-projectile))

;; Company Mode Notes
;; http://company-mode.github.io/
;; Once installed, enable company-mode with M-x company-mode.

;; Completion will start automatically after you type a few
;; letters. Use M-n and M-p to select, <return> to complete or <tab>
;; to complete the common part. Search through the completions with
;; C-s, C-r and C-o. Press M-(digit) to quickly complete with one of
;; the first 10 candidates.

;; Type M-x company-complete to initiate completion manually. Bind
;; this command to a key combination of your choice.

(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook (lambda () (whitespace-mode -1)))

(add-hook 'before-save-hook 'gofmt-before-save)
(add-to-list 'interpreter-mode-alist
             '("gorun" . go-mode))
