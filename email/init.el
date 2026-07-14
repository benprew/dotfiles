;; configs for reading email in emacs

(dolist (dir (if (eq system-type 'darwin)
                 '("/opt/homebrew/share/emacs/site-lisp/mu/mu4e")
               '("/usr/share/emacs/site-lisp/mu4e"
                 "/usr/share/emacs/site-lisp/mu/mu4e")))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)))

(use-package mu4e
  :ensure nil
  :demand t
  :config
  ;; 1. General Settings
  (setq mu4e-change-filenames-when-moving t   ; Recommended for mbsync
        mu4e-update-interval (* 5 60)         ; Sync every 5 minutes
        mu4e-get-mail-command "mbsync gmail"  ; Command to fetch mail
        mu4e-maildir "~/Mail/gmail"           ; Top-level mail directory
        mu4e-attachment-dir "~/Downloads"
        ;; Moving to Gmail's Trash is sufficient.  Do not also set the
        ;; Maildir \Trashed flag; mbsync would translate that into a second,
        ;; provider-dependent delete operation.
        mu4e-trash-without-flag t)

  ;; 2. Account Contexts (Gmail specific)
  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "Gmail"
          :match-func (lambda (msg)
                        (when msg
                          ;; `mu4e-maildir' is already ~/Mail/gmail, so mu's
                          ;; maildir names are /INBOX, /all, etc.
                          (string-prefix-p "/" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address  . "ben@throwingbones.com")
                  (user-full-name     . "Ben Prew")
                  (mu4e-sent-folder   . "/sent")
                  (mu4e-trash-folder  . "/trash")
                  (mu4e-drafts-folder . "/drafts")
                  ;; Refiling (`r') archives by removing the Inbox label;
                  ;; Gmail retains the message in All Mail.
                  (mu4e-refile-folder . "/all")
                  ;; Gmail creates the Sent Mail copy after SMTP submission.
                  (mu4e-sent-messages-behavior . delete)
                  ;; Sending configuration (using msmtp)
                  (sendmail-program   . "/usr/bin/msmtp")
                  (message-send-mail-function . message-send-mail-with-sendmail)
                  (message-sendmail-f-is-evil . t)
                  (message-sendmail-extra-arguments . ("--read-envelope-from"))))))

  ;; 3. Compose settings
  (add-hook 'mu4e-compose-mode-hook (lambda ()
                                      (auto-fill-mode -1)
                                      (visual-line-mode 1)))

  ;; 4. Keybindings
  (define-key mu4e-main-mode-map (kbd "j") 'mu4e-jump-to-maildir)
  (define-key mu4e-main-mode-map (kbd "G") 'my/mu4e-compose-to-group)

  (setq mu4e-maildir-shortcuts
        '((:maildir "/INBOX" :key ?i)
          (:maildir "/all"   :key ?a)
          (:maildir "/sent"  :key ?s)
          (:maildir "/trash" :key ?t)))

  ;; Bookmarks: exclude trashed and archived/All Mail copies. Since 'd' no
  ;; longer sets the \Trashed flag, the usual "NOT flag:trashed" filter won't
  ;; catch it, so exclude the Trash maildir by name instead.
  (setq mu4e-bookmarks
        '((:name "Inbox"               :query "maildir:/INBOX" :key ?i :favorite t)
          (:name "Unread messages"     :query "flag:unread AND NOT maildir:/trash AND NOT maildir:/all" :key ?u)
          (:name "Today's messages"    :query "date:today..now AND NOT maildir:/trash AND NOT maildir:/all" :key ?t)
          (:name "Last 7 days"         :query "date:7d..now AND NOT maildir:/trash AND NOT maildir:/all" :key ?w)
          (:name "Messages with images" :query "mime:image/* AND NOT maildir:/trash AND NOT maildir:/all" :key ?p)))

  ;; 5. Integration with Org-Mode
  (require 'mu4e-org)
  (setq mu4e-org-contacts-file "~/org/contacts.org")

  ;; 5. Auto-add contacts from messages
  (add-to-list 'mu4e-headers-actions
               '("org-contact-add" . mu4e-action-add-org-contact) t)
  (add-to-list 'mu4e-view-actions
               '("org-contact-add" . mu4e-action-add-org-contact) t))

;; org-contacts: manage contacts in org-mode, integrates with mu4e
(use-package org-contacts
  :ensure t
  :after org
  :config
  (setq org-contacts-files '("~/org/contacts.org"))

  ;; Add org-contacts to capture templates for quick contact creation
  (add-to-list 'org-capture-templates
               '("c" "Contact" entry (file "~/org/contacts.org")
                 "* %^{Name}
:PROPERTIES:
:EMAIL: %^{Email}
:GROUP: %^{Group|friends|family|work|other}
:END:"
                 :empty-lines 1)
               t)

  ;; Enable org-contacts completion in mu4e compose
  (add-to-list 'message-completion-alist
               '("^\\(To\\|Cc\\|Bcc\\):" . org-contacts-message-complete-function)
               t)

  ;; Compose to a contact group by :GROUP: property
  (defun my/mu4e-compose-to-group (group)
    "Compose an email to all org-contacts in GROUP."
    (interactive
     (list (completing-read "Group: "
                            (seq-uniq
                             (org-map-entries
                              (lambda () (org-entry-get nil "GROUP"))
                              "GROUP={.+}" org-contacts-files)))))
    (let ((emails (org-map-entries
                   (lambda () (org-entry-get nil "EMAIL"))
                   (format "GROUP={%s}" group) org-contacts-files)))
      (compose-mail (string-join emails ", "))))

)
