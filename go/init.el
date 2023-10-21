(require 'use-package)

(use-package go-playground
  :ensure t
  :defer 3)

(use-package go-mode
  :ensure t
  :hook ('go-mode . 'eglot-ensure))

; todo: is this needed?
; (add-hook 'go-mode-hook 'go-eldoc-setup)
; (add-hook 'go-mode-hook (lambda () (whitespace-mode -1)))

(add-hook 'before-save-hook 'gofmt-before-save)
(add-to-list 'interpreter-mode-alist
             '("gorun" . go-mode))

;; elgot config - from https://go.googlesource.com/tools/+/refs/heads/master/gopls/doc/emacs.md
(require 'project)

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)

;; I'm not sure what this is for?
(setq-default eglot-workspace-configuration
              '((:gopls .
                        ((staticcheck . t)
                         (matcher . "CaseSensitive")))))

;; organize go import statements
(defun own/eglot-organize-imports ()
  (call-interactively 'eglot-code-action-organize-imports))
(defun own/before-saving-go ()
  ;; you might also like:
  ;; (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
  (add-hook 'before-save-hook #'own/eglot-organize-imports nil t))
(add-hook 'go-mode-hook #'own/before-saving-go)
;; end eglot config
