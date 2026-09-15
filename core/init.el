;;; -*- lexical-binding: t; -*-

(global-set-key (kbd "C-c e n") 'flymake-goto-next-error)
(global-set-key (kbd "C-c e p") 'flymake-goto-prev-error)
(global-set-key (kbd "C-;") 'comment-dwim)

;; automatically save buffers when focus is lost
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

;; Emacs defauls behave more like VS-Code defaults
;; Kill current buffer (instead of asking first buffer name)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(delete-selection-mode 1)

;; Open recent files
(global-set-key (kbd "C-c r") 'recentf-open-files)

;; Enable auto-pair globally
(electric-pair-mode 1)

(setq initial-scratch-message ";Don't ignore your dreams\n;Don't work too much\n;Say what you think\n;Cultivate friendships\n;Be happy.\n\n")
(setq speedbar-show-unknown-files t)
(setq speedbar-use-images t) ;; optional, shows nice icons
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(setq confirm-kill-emacs 'y-or-n-p)
(setq-default fill-column 82)
(setq-default whitespace-line-column 82)
(global-auto-revert-mode t)

(use-package graphviz-dot-mode
  :defer t
  :ensure t)

(use-package jq-mode
  :defer t
  :ensure t
  :mode "\\.jq\\'")

(defun flymake-jq-backend (report-fn &rest _args)
  "Flymake backend using jq for JSON validation."
  (when (executable-find "jq")
    (let ((temp-file (make-temp-file "flymake-jq" nil ".json"))
          (source-buffer (current-buffer)))
      (write-region (point-min) (point-max) temp-file nil 'silent)
      (let ((proc (make-process
                   :name "flymake-jq"
                   :buffer (generate-new-buffer " *flymake-jq*")
                   :command (list "jq" "." temp-file)
                   :connection-type 'pipe
                   :sentinel
                   (lambda (proc _event)
                     (when (eq 'exit (process-status proc))
                       (let ((temp-file (process-get proc 'temp-file))
                             (report-fn (process-get proc 'report-fn))
                             (source-buffer (process-get proc 'source-buffer)))
                         (unwind-protect
                             (if (zerop (process-exit-status proc))
                                 (funcall report-fn nil)
                               (with-current-buffer (process-buffer proc)
                                 (goto-char (point-min))
                                 (let ((diags))
                                   (while (re-search-forward "parse error: \\(.+\\) at line \\([0-9]+\\)" nil t)
                                     (let ((msg (match-string 1))
                                           (line (string-to-number (match-string 2))))
                                       (push (flymake-make-diagnostic
                                              source-buffer
                                              (with-current-buffer source-buffer
                                                (save-excursion
                                                  (goto-char (point-min))
                                                  (forward-line (1- line))
                                                  (point)))
                                              (with-current-buffer source-buffer
                                                (save-excursion
                                                  (goto-char (point-min))
                                                  (forward-line (1- line))
                                                  (line-end-position)))
                                              :error
                                              msg)
                                             diags)))
                                   (funcall report-fn diags))))
                           (kill-buffer (process-buffer proc))
                           (when (file-exists-p temp-file)
                             (delete-file temp-file)))))))))
        (process-put proc 'temp-file temp-file)
        (process-put proc 'report-fn report-fn)
        (process-put proc 'source-buffer source-buffer)
        (process-put proc 'flymake-backend t)))))
;; Add the backend to JSON modes
(defun setup-json-flymake ()
  "Setup flymake with jq for JSON files."
  (add-hook 'flymake-diagnostic-functions #'flymake-jq-backend nil t)
  (flymake-mode 1))

(treesit-add-language-source 'json "https://github.com/tree-sitter/tree-sitter-json")
(add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))
(add-hook 'js-json-mode-hook #'setup-json-flymake)
(add-hook 'json-ts-mode-hook #'setup-json-flymake)

(treesit-add-language-source 'yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
;; (add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))

(use-package dumb-jump
  :defer t
  :ensure t
  :config (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package ssh-config-mode
  :defer t
  :ensure t
  :config
  (autoload 'ssh-config-mode "ssh-config-mode" t)
  :mode ("/\\.ssh/config\\'" "/rap_ssh_config\\'" "/sshd?_config\\'"
   "/knownhosts\\'" "/authorized_keys2?\\'" "_ssh_config\\'")
  :hook turn-on-font-lock)

(use-package crontab-mode
  :ensure t
  :defer t
  :mode "\\.cron\\'")

(add-to-list 'auto-mode-alist '("gitconfig\\.symlink\\'" . gitconfig-mode))
(add-to-list 'auto-mode-alist '("\\.el\\.symlink\\'" . emacs-lisp-mode))

(use-package helpful
  :ensure t
  :defer t
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function))

(use-package dired
  :ensure nil
  :config
  (when (eq system-type 'darwin)
    (setq insert-directory-program "gls")))

;; zeal is like dash documentation, but for linux
(use-package zeal-at-point
  :ensure t
  :defer t)
(global-set-key "\C-cd" 'zeal-at-point)

;; Add magit to project.el selection
(with-eval-after-load 'project
  (define-key project-prefix-map "m" #'magit-project-status)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))

(use-package eglot-hierarchy
  :vc (:url "https://github.com/dolmens/eglot-hierarchy"
       :rev :newest)
  :after eglot)

(use-package consult
  :ensure t
  :commands consult-ripgrep
  :init
  (with-eval-after-load 'project
    (keymap-set project-prefix-map "/" #'consult-ripgrep))
  :custom
  (consult-async-min-input 2)
  (consult-async-input-throttle 0.1)
  (consult-async-input-debounce 0.05)
  (consult-async-refresh-delay 0.05))
