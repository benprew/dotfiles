(prelude-require-packages '(ledger-mode flycheck-ledger))

;; Disable whitespace-mode when using ledger
(add-hook 'ledger-mode-hook (lambda () (whitespace-mode -1)))
