(use-package magit
  :ensure t
  :commands (magit-status magit-project-status))

(use-package pinentry
  :ensure t
  :config
  (pinentry-start))
