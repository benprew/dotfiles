;;; -*- lexical-binding: t; -*-

(use-package go-playground
  :ensure t
  :commands go-playground)

(use-package go-mode
  :ensure t
  :hook (go-mode . eglot-ensure)
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
  (when-let* ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)

(defun own/before-saving-go ()
  "Organize imports and format a Go buffer managed by Eglot."
  ;; gopls reports an error when there are no import edits to apply.
  (ignore-errors
    (call-interactively #'eglot-code-action-organize-imports))
  (eglot-format-buffer))

(defun own/setup-go-save-hook-after-eglot ()
  "Update the save hook after Eglot starts or stops managing Go."
  (when (derived-mode-p 'go-mode 'go-ts-mode)
    (if (eglot-managed-p)
        (add-hook 'before-save-hook #'own/before-saving-go nil t)
      (remove-hook 'before-save-hook #'own/before-saving-go t))))

(add-hook 'go-ts-mode-hook #'eglot-ensure)
(add-hook 'eglot-managed-mode-hook #'own/setup-go-save-hook-after-eglot)
