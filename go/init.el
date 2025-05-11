(require 'use-package)

(use-package go-playground
  :ensure t
  :defer 3)

(use-package go-mode
  :ensure t
  :hook ('go-mode . 'eglot-ensure)
  :bind (:map go-mode-map
              ("C-c t t" . go-test-current-test)
              ("C-c t f" . go-test-current-file)
              ("C-c t p" . go-test-current-project))
  )

(add-to-list 'interpreter-mode-alist
             '("gorun" . go-mode))

;; elgot config -
;; from https://go.googlesource.com/tools/+/refs/heads/master/gopls/doc/emacs.md
;;
;; Configuring project for Go modules in .emacs
;;
;; Eglot uses the built-in project package to identify the LSP workspace for a
;; newly-opened buffer. The project package does not natively know about GOPATH or
;; Go modules. Fortunately, you can give it a custom hook to tell it to look for
;; the nearest parent go.mod file (that is, the root of the Go module) as the
;; project root.
(require 'project)

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)

;; organize go import statements
(defun own/eglot-organize-imports ()
  (call-interactively 'eglot-code-action-organize-imports))
(defun own/before-saving-go ()
  ;; install eglot-format-buffer as a save hook.
  ;; The depth of -10 places this before eglot's willSave notification,
  ;; so that that notification reports the actual contents that will be saved.
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
  (add-hook 'before-save-hook #'own/eglot-organize-imports nil t))
(add-hook 'go-mode-hook #'own/before-saving-go)
