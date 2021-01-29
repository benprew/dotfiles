(setq flycheck-python-pycompile-executable "python3")
(setq flycheck-python-flake8-executable "python3")

(require 'use-package)

(use-package prelude-python
  :defer 5)

(use-package blacken
  :defer t
  :ensure t
  :hook (python-mode . blacken-mode))

(use-package jinja2-mode
  :defer t
  :ensure t
  :mode "\\.jt'")
