;;; org.el -- ORG MODE SETTINGS
;;; Commentary:
;;;   This contains all my org-mode settings

;;; Code:
(require 'use-package)

(use-package org
  :bind (:map org-mode-map
         ("C-c t e" . org-table-export)))

;; (setq org-agenda-files '("~/notes/"))
;; Too many org files can cause slowdown in agenda building
;; see https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html for a way to limit files
(setq org-agenda-files (directory-files-recursively "~/notes/" "\\`[^.].*\\.org\\'"))
(setq org-reverse-note-order t)
(setq org-refile-targets '((nil :maxlevel . 3) (org-agenda-files :maxlevel . 3)))
(setq org-startup-indented t)
(setq org-export-with-sub-superscripts '{})
(add-hook 'org-mode-hook (lambda () (whitespace-mode -1)))
(add-hook 'org-mode-hook #'visual-line-mode)
;; bigger latex fragment -- needs to be run once org is loaded?
;; (plist-put org-format-latex-options :scale 1.5)

(setq org-export-backends '(ascii html icalendar latex md odt))

(setq org-log-done t)  ; add timestamp when completing a todo item

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("sql" . "src sql"))
(add-to-list 'org-structure-template-alist '("as-sql" . "src sql :engine postgres :dbhost as-linear.db.csas.csa.comscore.com :dbport 5439 :dbuser bprew :database live"))

(setq org-todo-keywords
      '((sequence "TODO(t)" "IN PROGRESS(i)" "|" "DONE(d)")
        (sequence "LATER(l)" "|" "NEVER")))


(setq org-agenda-custom-commands
      '(("t" todo "TODO" nil)
        ("i" todo "IN PROGRESS" nil)
        ("l" todo "LATER" nil)
        ("d" todo "DONE" nil)
        ("n" todo "NEVER" nil)))

(use-package visual-fill-column
  :ensure t
  :config
  ;; unset because it gets into a loop on linliveanalytics1
  (if (and (> emacs-major-version 25) (display-graphic-p))
      (add-hook 'org-mode-hook #'visual-fill-column-mode)))

(use-package markdown-mode
  :ensure t
  :defer 1
  :mode "\\.md\\'"
  :hook
  (markdown-mode . visual-line-mode)
  (markdown-mode . (lambda () (whitespace-mode -1)))
  :config
  ;; Markdown formatting
  (setq markdown-gfm-use-electric-backquote nil))

(use-package org-preview-html
  :ensure t
  :defer 1)

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance '("crypt"))

;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key "FEEB1B3FD867173D8BCCD26C7E66F5759AFC82E0")

;; Auto-saving does not cooperate with org-crypt.el: so you need to
;; turn it off if you plan to use org-crypt.el quite often.  Otherwise,
;; you'll get an (annoying) message each time you start Org.
;; (setq auto-save-default nil)

(require 'epa-file)
(epa-file-enable)
(setq epa-file-select-keys "ben@throwingbones.com")

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)
   (dot . t)
   (python . t)
   (shell . t)))

(add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "dot")))  ; don't ask for dot
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
