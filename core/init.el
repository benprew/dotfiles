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
(setq custom-file "~/.emacs.d/custom.el")
(setq load-prefer-newer 't)
(setq whitespace-mode 1)
(setq column-number-mode t)
(global-auto-revert-mode t)

;; save backup files into a single directory
(make-directory "~/.emacs_backups/" t)
(make-directory "~/.emacs_autosave/" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs_autosave/" t)))
(setq backup-directory-alist '(("." . "~/.emacs_backups/")))
(setq create-lockfiles nil)

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

;; Only install tree-sitter grammars if not already available
(unless (treesit-language-available-p 'json)
  (treesit-add-and-install 'json "https://github.com/tree-sitter/tree-sitter-json"))
(add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))
(add-hook 'js-json-mode-hook #'setup-json-flymake)
(add-hook 'json-ts-mode-hook #'setup-json-flymake)

(unless (treesit-language-available-p 'yaml)
  (treesit-add-and-install 'yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml"))
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

(use-package eglot
  :ensure t
  :defer 2
  :hook (python-mode . eglot-ensure)(go-mode . eglot-ensure))

;; zeal is like dash documentation, but for linux
(use-package zeal-at-point
  :ensure t
  :defer t)
(global-set-key "\C-cd" 'zeal-at-point)

;; Add magit to project.el selection
(with-eval-after-load 'project
  (define-key project-prefix-map "m" #'magit-project-status)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))

;; Improved search function (use f2) in isearch to show
;; From http://yummymelon.com/devnull/improving-emacs-isearch-usability-with-transient.html
;; (require 'transient)
;; (transient-define-prefix cc/isearch-menu ()
;;   "Isearch Menu."
;;   [["Edit Search String"
;;     ("e"
;;      "Edit the search string (recursive)"
;;      isearch-edit-string
;;      :transient nil)
;;     ("w"
;;      "Pull next word or character word from buffer"
;;      isearch-yank-word-or-char
;;      :transient nil)
;;     ("s"
;;      "Pull next symbol or character from buffer"
;;      isearch-yank-symbol-or-char
;;      :transient nil)
;;     ("l"
;;      "Pull rest of line from buffer"
;;      isearch-yank-line
;;      :transient nil)
;;     ("y"
;;      "Pull string from kill ring"
;;      isearch-yank-kill
;;      :transient nil)
;;     ("t"
;;      "Pull thing from buffer"
;;      isearch-forward-thing-at-point
;;      :transient nil)]
;;
;;    ["Replace"
;;     ("q"
;;      "Start ‘query-replace’"
;;      isearch-query-replace
;;      :if-nil buffer-read-only
;;      :transient nil)
;;     ("x"
;;      "Start ‘query-replace-regexp’"
;;      isearch-query-replace-regexp
;;      :if-nil buffer-read-only
;;      :transient nil)]]
;;
;;   [["Toggle"
;;     ("X"
;;      "Toggle regexp searching"
;;      isearch-toggle-regexp
;;      :transient nil)
;;     ("S"
;;      "Toggle symbol searching"
;;      isearch-toggle-symbol
;;      :transient nil)
;;     ("W"
;;      "Toggle word searching"
;;      isearch-toggle-word
;;      :transient nil)
;;     ("F"
;;      "Toggle case fold"
;;      isearch-toggle-case-fold
;;      :transient nil)
;;     ("L"
;;      "Toggle lax whitespace"
;;      isearch-toggle-lax-whitespace
;;      :transient nil)]
;;
;;    ["Misc"
;;     ("o"
;;      "occur"
;;      isearch-occur
;;      :transient nil)]])
;;
;; (define-key isearch-mode-map (kbd "<f2>") 'cc/isearch-menu)

;; End improved search
