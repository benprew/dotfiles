(require 'use-package)

(setq js-indent-level 2)
(local-set-key (kbd "RET") 'newline-and-indent)

(use-package prelude-web
  :defer 3)

(use-package prelude-js
  :defer 3)

(use-package web-mode
  :defer 3
  :ensure t
  :mode ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.blade\\.php\\'"
         "\\.jsp\\'"  "\\.as[cp]x\\'"  "\\.erb\\'"  "\\.html?\\'"
         "/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'")
  :config  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-disable-autocompletion t))
