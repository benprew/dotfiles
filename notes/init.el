;;; org.el -- ORG MODE SETTINGS
;;; Commentary:
;;;   This contains all my org-mode settings

;;; Code:
(prelude-require-packages '(visual-fill-column))

(require 'prelude-org)

(setq org-agenda-files '("~/notes/"))
(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))
(setq org-startup-indented t)
(setq org-return-follows-link t) ; I prefer return to activate a link
(add-hook 'org-mode-hook (lambda () (whitespace-mode -1)))
(add-hook 'org-mode-hook #'visual-line-mode)
;; unset because it gets into a loop on linliveanalytics1
(if (and (> emacs-major-version 25) (display-graphic-p))
  (add-hook 'org-mode-hook #'visual-fill-column-mode))

(add-hook 'markdown-mode-hook (lambda () (whitespace-mode -1)))
(add-hook 'markdown-mode-hook #'visual-line-mode)
;; unset because it gets into a loop on linliveanalytics1
(if (and (> emacs-major-version 25) (display-graphic-p))
    (add-hook 'markdown-mode-hook #'visual-fill-column-mode))

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

(setq markdown-gfm-use-electric-backquote nil)

;; bigger latex fragment -- needs to be run once org is loaded?
;; (plist-put org-format-latex-options :scale 1.5)
;;; org.el ends here
