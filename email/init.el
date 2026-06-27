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
        mu4e-attachment-dir "~/Downloads")

  ;; 2. Account Contexts (Gmail specific)
  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "Gmail"
          :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address  . "ben@throwingbones.com")
                  (user-full-name     . "Ben Prew")
                  ;; Recognize both addresses as "me" so replies don't loop back
                  ;; to myself and so the "from me" detection works on either.
                  (mu4e-personal-addresses . ("ben@throwingbones.com"
                                              "ben.prew@gmail.com"))
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

  ;; 4. Keybindings
  (define-key mu4e-main-mode-map (kbd "j") 'mu4e-jump-to-maildir)
  (define-key mu4e-main-mode-map (kbd "G") 'my/mu4e-compose-to-group)

  ;; Gmail-friendly trash: 'd' moves to the Trash folder WITHOUT setting the
  ;; \Deleted (+T) flag. Setting +T makes Gmail misbehave (trashed mail
  ;; reappears in the inbox); moving to the folder is enough for Gmail.
  (setf (alist-get 'trash mu4e-marks)
        '(:char ("d" . "▼")
          :prompt "dtrash"
          :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
          :action (lambda (docid msg target)
                    (mu4e--server-move
                     docid (mu4e--mark-check-target target) "+S-N"))))

  ;; Bookmarks: exclude trashed mail. Since 'd' no longer sets the \Trashed
  ;; flag, the usual "NOT flag:trashed" filter won't catch it, so exclude the
  ;; Trash maildir by name instead.
  (setq mu4e-bookmarks
        '((:name "Unread messages"     :query "flag:unread AND NOT maildir:/trash" :key ?u)
          (:name "Today's messages"    :query "date:today..now AND NOT maildir:/trash" :key ?t)
          (:name "Last 7 days"         :query "date:7d..now AND NOT maildir:/trash" :key ?w)
          (:name "Messages with images" :query "mime:image/* AND NOT maildir:/trash" :key ?p)))

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
