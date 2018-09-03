;;; org.el -- ORG MODE SETTINGS
;;; Commentary:
;;;   This contains all my org-mode settings

;;; Code:
(setq org-agenda-files '("~/notes/"))
(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))
(setq org-startup-indented t)
(setq org-return-follows-link t) ; I prefer return to activate a link

(setq org-todo-keywords
      '((sequence "INBOX(i)"
                  "TODAY(t)"
                  "TOMORROW(o)"
                  "SOMEDAY(s)"
                  "WAITING(w)"
                  "DONE(d)")))

(setq org-agenda-custom-commands
      '(("w" todo "WAITING" nil)
        ("i" todo "INBOX" nil)
        ("t" todo "TODAY" nil)
        ("o" todo "TOMORROW" nil)
        ("s" todo "SOMEDAY" nil)
        ("d" "Agenda + Next Actions" ((agenda) (todo "INBOX") (todo "TODAY")))))

; Jump to gtd file
(defun gtd ()
  (interactive)
  (find-file "~/notes/gtd.org"))

;;; org.el ends here
