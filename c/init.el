;;; c/init.el --- C and C99 configuration with Eglot LSP -*- lexical-binding: t; -*-

(use-package eglot
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs
               '((c-mode c-ts-mode)
                 . ("clangd"
                    :initializationOptions
                    (:fallbackFlags ["-std=c99"]))))
  :hook ((c-mode c-ts-mode) . eglot-ensure))

(defun own/c-format-on-save ()
  "Format C buffer through Eglot when supported."
  (when (and (eglot-managed-p)
             (eglot-server-capable :documentFormattingProvider))
    (eglot-format-buffer)))

(defun own/setup-c-format-on-save ()
  (add-hook 'before-save-hook #'own/c-format-on-save nil t))

(dolist (hook '(c-mode-hook c-ts-mode-hook))
  (add-hook hook #'own/setup-c-format-on-save))

(when (fboundp 'treesit-add-language-source)
  (treesit-add-language-source 'c "https://github.com/tree-sitter/tree-sitter-c")
  (treesit-add-language-source 'cpp "https://github.com/tree-sitter/tree-sitter-cpp"))

;; Remap c-mode to c-ts-mode only when tree-sitter grammars (c and cpp) are available
(when (and (fboundp 'treesit-language-available-p)
           (treesit-language-available-p 'c)
           (treesit-language-available-p 'cpp))
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode)))

(use-package c-ts-mode
  :ensure nil)
