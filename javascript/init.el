(require 'prelude-js)
(require 'prelude-web)

(setq js2-basic-offset 2)
(local-set-key (kbd "RET") 'newline-and-indent)

(use-package web-mode
  :mode ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.blade\\.php\\'"
         "\\.jsp\\'"  "\\.as[cp]x\\'"  "\\.erb\\'"  "\\.html?\\'"
         "/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'")
  :init  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-disable-autocompletion t)
  ; Disable whitespace-mode when using web-mode
  (add-hook 'web-mode-hook (lambda () (whitespace-mode -1))))
