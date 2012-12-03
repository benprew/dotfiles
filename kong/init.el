(require-el-get '(:name actionscript-mode
                        :description "Major mode for editing ActionScript files"
                        :type git
                        :url "https://github.com/austinhaas/actionscript-mode.git"
                        :after (progn
                                 (autoload 'actionscript-mode "actionscript-mode" "Major mode for editing ActionScript files" t)
                                 (add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode)))))


(defun kong-test-server ()
  (interactive)
  (multi-term-shell-command
   "ssh -t kongdev bash -c \"export LC_ALL='en_US.UTF-8' && cd /k/kongregate/current && exec script/test_server\""
   "*test-server*"))

(defun kong-console ()
  (interactive)
  (remote-shell-command
   "kongdev"
   "bash -c \"export LC_ALL='en_US.UTF-8' && cd /k/kongregate/current && exec script/console\""
   "*console*"))

(defun kong-kill-test-server ()
  (interactive)
  (shell-command (concat "ssh kongdev " (shell-quote-argument "ps aux | grep test_server | grep -v ssh | grep -v grep | awk '{print $2}' | xargs kill -9")))
  (kill-buffer "*test-server*"))

(defun kong-run-test-file (file)
  (interactive "MTest File: ")
  (compile (format "ssh kongdev 'export LC_ALL=en_US.UTF-8 && cd /k/kongregate/current && exec ruby %s'" file)))

(defun kong-run-current-test-file ()
  (interactive)
  (kong-run-test-file (kong-current-relative-file-name)))

(defun kong-run-test-at-point ()
  (interactive)
  (compile (format "ssh kongdev 'export LC_ALL=en_US.UTF-8 && cd /k/kongregate/current && exec ruby %s -n %s'" (kong-current-relative-file-name) (rinari-test-function-name))))

(defun kong-current-relative-file-name ()
  (file-relative-name (buffer-file-name) (getenv "KONGROOT")))

(defun growl (message)
  (interactive "M")
  (shell-command (concat (getenv "DOTFILESROOT") "/kong/bin/compile-notify.rb " (shell-quote-argument message))))

(defun growl-compilation-result(buffer msg)
  (growl (concat (buffer-name buffer) " " msg)))

(post-init (lambda ()
  (add-to-list 'compilation-finish-functions 'growl-compilation-result)
  (set-face-font 'default "Meslo LG L 14")
  (setenv "KONGROOT" "/Users/benprew/src/kongregate")

  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        mail-host-address "bprew.kongregate.com"
        smtpmail-local-domain "kongregate.com")

  (define-key rinari-minor-mode-map "\C-c." 'kong-run-test-at-point)
  (define-key rinari-minor-mode-map "\C-ct" 'kong-run-current-test-file)
  (define-key rinari-minor-mode-map "\C-cr" 'recompile)

  (setq smtpmail-auth-credentials "/Users/benprew/.authinfo")
  (setq starttls-gnutls-program "/usr/local/bin/gnutls-cli")

  (color-theme-solarized-dark)
))
