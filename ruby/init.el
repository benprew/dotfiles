(use-package rubocop
  :ensure t
  :defer 3
  :after ruby-mode
  :hook
  (ruby-mode . rubocop-mode)
  :config
  (setq rubocop-format-on-save t))

; C-c C-s - launch the inf-ruby process.
; highlight a chunk of code, C-c C-r - push that Ruby code into the IRB session.
;  For example, try defining a class in your Ruby buffer, select the whole buffer, run C-c C-r, then swap over to the inf-ruby buffer and instantiate an instance of your class. Pretty cool!
;  Alternatively, use C-c M-r to run a selected chunk of code and automatically go to the ruby buffer
;  If you do a lot of work in Rails or Sinatra, check out the commands inf-ruby-console-rails and inf-ruby-console-racksh. Using these commands inf-ruby can start a console session in the environment of your web project.
(use-package inf-ruby
  :ensure t
  :defer 3
  :after ruby-mode
  :hook
  (ruby-mode . inf-ruby-minor-mode))

(add-hook 'ruby-mode-hook (lambda ()
			                (setq eglot-stay-out-of '(flymake))
                            (flymake-mode t)
			                (eglot-ensure)))


;; https://emacs-lsp.github.io/lsp-mode/page/lsp-solargraph/
; (setq lsp-solargraph-use-bundler 't)
;
; (defun lsp-ruby-install-save-hooks ()
;   (add-hook 'before-save-hook #'lsp-format-buffer t t))
; (add-hook 'ruby-mode-hook #'lsp-ruby-install-save-hooks)


;; ruby-lsp doesn't work in eglot
;; eglot--error: [eglot] Sorry, this server doesn't do :textDocument/definition
;(add-to-list 'eglot-server-programs
;             `(ruby-mode . ("ruby-lsp")))

;; solargraph and eglot don't report errors, so turn off eglot-flymake and use default
;; rubymode flymake
