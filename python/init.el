(require 'prelude-python)
(prelude-require-packages '(blacken))
(add-hook 'python-mode-hook 'blacken-mode)
