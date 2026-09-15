;;; -*- lexical-binding: t; -*-

(use-package flymake-shellcheck
  :ensure t
  :commands flymake-shellcheck-load
  :hook
  ((sh-mode bash-ts-mode) . flymake-shellcheck-load))

(use-package fish-mode
  :ensure t
  :mode ("\\.fish\\'" "\\.fish\\.symlink\\'"))

(treesit-add-language-source 'bash "https://github.com/tree-sitter/tree-sitter-bash")

(use-package sh-script
  :ensure nil
  :mode ("\\.sh\\'" . bash-ts-mode))

