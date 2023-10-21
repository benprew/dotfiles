(use-package rust-mode
  :ensure t)

(add-hook 'rust-mode-hook 'eglot-ensure)
