;;; -*- lexical-binding: t; -*-
; C-c C-s - launch the inf-ruby process.
; highlight a chunk of code, C-c C-r - push that Ruby code into the IRB session.
;  For example, try defining a class in your Ruby buffer, select the whole buffer, run C-c C-r, then swap over to the inf-ruby buffer and instantiate an instance of your class. Pretty cool!
;  Alternatively, use C-c M-r to run a selected chunk of code and automatically go to the ruby buffer
;  If you do a lot of work in Rails or Sinatra, check out the commands inf-ruby-console-rails and inf-ruby-console-racksh. Using these commands inf-ruby can start a console session in the environment of your web project.
(use-package inf-ruby
  :ensure t
  :hook
  ((ruby-mode ruby-ts-mode) . inf-ruby-minor-mode))

(use-package eglot
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs
               '((ruby-mode ruby-ts-mode) "ruby-lsp"))
  :hook ((ruby-mode ruby-ts-mode) . eglot-ensure))


(defun own/ruby-format-on-save ()
  "Format Ruby through Ruby LSP when supported."
  (when (and (eglot-managed-p)
             (eglot-server-capable :documentFormattingProvider))
    (eglot-format-buffer)))

(defun own/setup-ruby-format-on-save ()
  (add-hook 'before-save-hook #'own/ruby-format-on-save nil t))

(dolist (hook '(ruby-mode-hook ruby-ts-mode-hook))
  (add-hook hook #'own/setup-ruby-format-on-save))



(treesit-add-language-source 'ruby "https://github.com/tree-sitter/tree-sitter-ruby")

;;; -------------------- TREESITTER AREA
;;; RUBY-TS-MODE
(use-package ruby-ts-mode
  :ensure nil
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :custom
  (ruby-indent-level 2)
  (ruby-indent-tabs-mode nil))
