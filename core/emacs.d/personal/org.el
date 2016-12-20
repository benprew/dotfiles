;;; org.el -- ORG MODE SETTINGS
;;; Commentary:
;;;   This contains all my org-mode settings

;;; Code:
(setq org-agenda-files '("~/notes/"))
(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))
(setq org-startup-indented t)
                                        ; I prefer return to activate a link
(setq org-return-follows-link t)

(setq org-agenda-custom-commands
      '(("w" todo "WAITING" nil)
        ("n" todo "NEXT" nil)
        ("d" "Agenda + Next Actions" ((agenda) (todo "NEXT"))))
      )

; Jump to gtd file
(defun gtd ()
  (interactive)
  (find-file "~/notes/gtd.org"))

;;; org.el ends here
