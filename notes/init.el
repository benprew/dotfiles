;;; org.el -- ORG MODE SETTINGS
;;; Commentary:
;;;   This contains all my org-mode settings

;;; Code:
(require 'use-package)

;; Agenda files setup - deferred until org-agenda is used
(defvar notes-dir-scanned nil
  "Whether we've scanned the notes directory for org-agenda-files.")

(defun notes-ensure-agenda-files ()
  "Scan notes directory for org files if not already done."
  (unless notes-dir-scanned
    (setq org-agenda-files (directory-files-recursively "~/notes/" "\\`[^.].*\\.org\\'"))
    (setq notes-dir-scanned t)))

(with-eval-after-load 'org-agenda
  (notes-ensure-agenda-files))

;; Main org-mode configuration
(use-package org
  :defer t
  :bind (:map org-mode-map
         ("C-c t e" . org-table-export))
  :hook
  ((org-mode . visual-line-mode)
   (org-mode . (lambda () (whitespace-mode -1)))
   (org-mode . flyspell-mode)
   (org-mode . (lambda ()
                 ;; Enable truncation when inside tables to prevent wrapping
                 (add-hook 'post-command-hook
                           (lambda ()
                             (if (org-at-table-p)
                                 (progn
                                   (setq-local truncate-lines t)
                                   (setq-local fill-column 200))
                               (setq-local truncate-lines nil)
                               (kill-local-variable 'fill-column)))
                           nil t))))
  :custom
  (org-reverse-note-order t)
  (org-refile-targets '((nil :maxlevel . 3) (org-agenda-files :maxlevel . 3)))
  (org-startup-indented t)
  (org-export-with-sub-superscripts '{})
  (org-export-backends '(ascii html icalendar latex md odt))
  (org-use-sub-superscripts nil)
  (org-log-done t)  ; add timestamp when completing a todo item
  (org-todo-keywords
   '((sequence "TODO(t)" "IN PROGRESS(i)" "|" "DONE(d)")
     (sequence "LATER(l)" "|" "NEVER")))
  (org-agenda-custom-commands
   '(("t" todo "TODO" nil)
     ("i" todo "IN PROGRESS" nil)
     ("l" todo "LATER" nil)
     ("d" todo "DONE" nil)
     ("n" todo "NEVER" nil))))

;; org-tempo for quick code block templates (<s, <e, etc.)
(use-package org-tempo
  :ensure nil
  :after org
  :config
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("sql" . "src sql"))
  (add-to-list 'org-structure-template-alist '("rb" . "src ruby"))
  (add-to-list 'org-structure-template-alist '("lisp" . "src lisp"))
  (add-to-list 'org-structure-template-alist '("as-sql" . "src sql :engine postgres :dbhost as-linear.db.csas.csa.comscore.com :dbport 5439 :dbuser bprew :database live")))

;; org-crypt for encrypting org subtrees
(use-package org-crypt
  :ensure nil
  :after org
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt"))
  (setq org-crypt-key "FEEB1B3FD867173D8BCCD26C7E66F5759AFC82E0"))

;; epa-file for handling encrypted files
(use-package epa-file
  :ensure nil
  :after org
  :config
  (epa-file-enable)
  (setq epa-file-select-keys "ben@throwingbones.com"))

;; ox-md for markdown export
(use-package ox-md
  :ensure nil
  :after ox)

;; ob (org-babel) for executing code blocks
(use-package ob
  :ensure nil
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t)
     (dot . t)
     (python . t)
     (shell . t)))

  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

  (defun my-org-confirm-babel-evaluate (lang body)
    "Don't ask for confirmation when evaluating dot code blocks."
    (not (string= lang "dot")))
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate))

;; visual-fill-column for better reading experience
(use-package visual-fill-column
  :ensure t
  :defer t
  :hook (org-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 82)  ;match fill-column length
  (visual-fill-column-center-text t))

;; org-preview-html for previewing org exports in browser
(use-package org-preview-html
  :ensure t
  :defer t
  :after org)

;; markdown-mode for editing markdown files
(use-package markdown-mode
  :ensure t
  :defer t
  :mode "\\.md\\'"
  :hook
  ((markdown-mode . visual-line-mode)
   (markdown-mode . flyspell-mode)
   (markdown-mode . (lambda () (whitespace-mode -1))))
  :custom
  (markdown-gfm-use-electric-backquote nil))
