(prelude-require-packages '(crontab-mode))

(add-to-list 'auto-mode-alist '("\\.cron$" . crontab-mode))
