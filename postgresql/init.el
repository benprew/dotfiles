(use-package sqlformat
  :defer 3
  :ensure t)


(require 'sqlformat)
(setq sqlformat-command 'pgformatter)
(setq sqlformat-args '("-g"))
