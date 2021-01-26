(prelude-require-packages '(sqlformat))

(require 'sqlformat)
(setq sqlformat-command 'pgformatter)
(setq sqlformat-args '("-s2" "-g"))
