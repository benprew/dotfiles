(require 'prelude-ruby)
(require 'prelude-web)

(setq lsp-solargraph-use-bundler 't)

(defun lsp-ruby-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t))
(add-hook 'ruby-mode-hook #'lsp-ruby-install-save-hooks)
