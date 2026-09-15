;;; -*- lexical-binding: t; -*-

(use-package magit
  :ensure t
  :commands (magit-status magit-project-status)
  :hook (git-commit-setup . (lambda () (setq fill-column 72))))

(use-package pinentry
  :ensure t
  :config
  (pinentry-start))
