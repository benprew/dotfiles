(use-package flymake-shellcheck
  :ensure t
  :commands flymake-shellcheck-load
  :hook
  ('sh-mode . 'flymake-shellcheck-load)
  ('sh-mode . 'flymake-mode))

(use-package fish-mode
  :defer 3
  :ensure t
  :mode ("\\.fish\\'" "\\.fish\\.symlink\\'"))

(use-package bash-ts-mode
  :ensure f
  :mode "\\.sh\\'")
