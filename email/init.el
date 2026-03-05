;; configs for reading email in emacs

(require 'mu4e)

(use-package mu4e
  :ensure nil
  ;; Point to where your system installed mu4e.
  ;; Common paths: "/usr/share/emacs/site-lisp/mu4e" or "/usr/local/share/emacs/site-lisp/mu/mu4e"
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :defer t
  :config
  ;; 1. General Settings
  (setq mu4e-change-filenames-when-moving t   ; Recommended for mbsync
        mu4e-update-interval (* 5 60)         ; Sync every 5 minutes
        mu4e-get-mail-command "mbsync gmail"  ; Command to fetch mail
        mu4e-maildir "~/Mail/gmail"           ; Top-level mail directory
        mu4e-attachment-dir "~/Downloads")

  ;; 2. Account Contexts (Gmail specific)
  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "Gmail"
          :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address  . "ben.prew@gmail.com")
                  (user-full-name     . "Ben Prew")
                  (mu4e-sent-folder   . "/sent")
                  (mu4e-trash-folder  . "/trash")
                  (mu4e-drafts-folder . "/drafts")
                  (mu4e-refile-folder . "/all")
                  ;; Sending configuration (using msmtp)
                  (sendmail-program   . "/usr/bin/msmtp")
                  (message-send-mail-function . message-send-mail-with-sendmail)
                  (message-sendmail-f-is-evil . t)
                  (message-sendmail-extra-arguments . ("--read-envelope-from"))))))

  ;; 3. Compose settings
  (add-hook 'mu4e-compose-mode-hook (lambda ()
                                      (auto-fill-mode -1)
                                      (visual-line-mode 1)))

  ;; 4. Useful Keybindings
  (define-key mu4e-main-mode-map (kbd "j") 'mu4e-jump-to-maildir)

  ;; 4. Integration with Org-Mode (The Killer Feature)
  (require 'mu4e-org)
  (setq mu4e-org-contacts-file  "~/org/contacts.org"))
