(require 'use-package)

(use-package ledger-mode
  :ensure t
  :defer t
  :config (add-hook 'ledger-mode (lambda () (setq-local prelude-clean-whitespace-on-save 1))))
