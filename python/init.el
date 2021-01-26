(require 'prelude-python)
(prelude-require-packages '(blacken jinja2-mode))
(add-hook 'python-mode-hook 'blacken-mode)
(setq flycheck-python-pycompile-executable "python3")
(setq flycheck-python-flake8-executable "python3")

(autoload 'jinja2-mode "jinja2-mode.el"
  "Major mode for editing jq files" t)
(add-to-list 'auto-mode-alist '("\\.jt$" . jinja2-mode))
