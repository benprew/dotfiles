(use-package python
  :ensure nil
  :defer t
  :bind ("C-c C-a" . btp/py-auto-eglot))

(use-package pyvenv
  :ensure t
  :defer t
  :init
  (setenv "WORKON_HOME" (expand-file-name "~/.pyenv/versions")))

(use-package blacken
  :ensure t
  :defer t
  :hook (python-base-mode . blacken-mode))

(use-package jinja2-mode
  :ensure t
  :mode "\\.jt\\'")


(defun dd/py-workon-project-venv ()
  "Activate the virtual environment named after the current project.
Return its full path, or nil when it does not exist."
  (let* ((project (project-current t))
         (project-name
          (file-name-nondirectory
           (directory-file-name (project-root project))))
         (venv (expand-file-name project-name (getenv "WORKON_HOME"))))
    (when (file-directory-p venv)
      (pyvenv-activate venv)
      venv)))

(defun btp/py-auto-eglot ()
  "Activate the project virtual environment and restart Eglot.
Prompt for an environment when none matches the project name."
  (interactive)
  (unless (dd/py-workon-project-venv)
    (call-interactively #'pyvenv-workon))
  (if-let ((server (eglot-current-server)))
      (eglot-reconnect server t)
    (eglot-ensure)))

(use-package eglot
  :ensure nil
  :config
  ;; Register ty for all built-in Python modes.
  (add-to-list 'eglot-server-programs
               '((python-base-mode :language-id "python") . ("ty" "server")))
  :hook (python-base-mode . eglot-ensure))
