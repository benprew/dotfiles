(require 'prelude-python)
(prelude-require-packages '(blacken))
(add-hook 'python-mode-hook 'blacken-mode)
(setq flycheck-python-pycompile-executable "python3")
(setq flycheck-python-flake8-executable "python3")
