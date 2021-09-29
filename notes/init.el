;;; org.el -- ORG MODE SETTINGS
;;; Commentary:
;;;   This contains all my org-mode settings

;;; Code:
(require 'use-package)

(setq org-agenda-files '("~/notes/"))
(setq org-reverse-note-order t)
(setq org-refile-targets '((nil :maxlevel . 3) (org-agenda-files :maxlevel . 3)))
(setq org-startup-indented t)
(add-hook 'org-mode-hook (lambda () (whitespace-mode -1)))
(add-hook 'org-mode-hook #'visual-line-mode)
;; bigger latex fragment -- needs to be run once org is loaded?
;; (plist-put org-format-latex-options :scale 1.5)

(setq org-export-backends '(ascii html icalendar latex md odt))


(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("sql" . "src sql"))
(add-to-list 'org-structure-template-alist '("as-sql" . "src sql :engine postgres :dbhost as-linear.db.csas.csa.comscore.com :dbport 5439 :dbuser bprew :database live"))

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

(use-package visual-fill-column
  :ensure t
  :defer 1
  :config
  ;; unset because it gets into a loop on linliveanalytics1
  (if (and (> emacs-major-version 25) (display-graphic-p))
      (add-hook 'org-mode-hook #'visual-fill-column-mode)))

(use-package markdown-mode
  :ensure t
  :defer 1
  :hook
  (markdown-mode . visual-line-mode)
  (markdown-mode . (lambda () (whitespace-mode -1)))
  :config
  ;; Markdown formatting
  (setq markdown-gfm-use-electric-backquote nil)
  ;; unset because it gets into a loop on linliveanalytics1
  (if (and (> emacs-major-version 25) (display-graphic-p))
      (add-hook 'markdown-mode-hook #'visual-fill-column-mode)))

(use-package prelude-org
  :defer 5)


;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)
   (python . t)
   (shell . t)))

;;; org.el ends here
