(require-el-get 'go-mode)

(post-init
 (lambda()
   (add-hook 'before-save-hook
             (lambda()
               (if (derived-mode-p 'go-mode)
                   (gofmt))))

   (add-hook 'go-mode-hook 'gofmt-on-save)
   (add-to-list 'interpreter-mode-alist
                '("gorun" . go-mode))))
