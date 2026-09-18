;;; -*- lexical-binding: t; -*-

(use-package python
  :ensure nil
  :defer t
  :bind ("C-c C-a" . btp/py-auto-eglot))

(use-package pyvenv
  :ensure t
  :defer t
  :init
  (let ((pyenv-versions (expand-file-name "~/.pyenv/versions")))
    (when (file-directory-p pyenv-versions)
      (setenv "WORKON_HOME" pyenv-versions))))

(use-package blacken
  :ensure t
  :defer t
  :hook (python-base-mode . blacken-mode))

(use-package jinja2-mode
  :ensure t
  :mode "\\.jt\\'")


;; Ensure ~/.local/bin is in exec-path and PATH for user-installed tools (ruff, ty, uv)
(let ((local-bin (expand-file-name "~/.local/bin")))
  (when (file-directory-p local-bin)
    (add-to-list 'exec-path local-bin)
    (setenv "PATH" (concat local-bin ":" (getenv "PATH")))))

(defun dd/py-workon-project-venv (&optional interactive)
  "Activate the virtual environment for the current project (.venv, venv, or pyenv).
Return its full path, or nil when it does not exist."
  (when-let* ((project (project-current interactive))
              (root (project-root project))
              (local-venv (expand-file-name ".venv" root))
              (local-venv2 (expand-file-name "venv" root))
              (project-name (file-name-nondirectory (directory-file-name root)))
              (workon-home (getenv "WORKON_HOME"))
              (pyenv-venv (and workon-home (expand-file-name project-name workon-home)))
              (venv (cond
                     ((file-directory-p local-venv) local-venv)
                     ((file-directory-p local-venv2) local-venv2)
                     ((and pyenv-venv (file-directory-p pyenv-venv)) pyenv-venv))))
    (pyvenv-activate venv)
    venv))

(defun btp/py-setup-venv ()
  "Activate the project virtual environment automatically if present.
Errors are caught and reported rather than signaled, since an
uncaught error here would abort the rest of `python-base-mode-hook'
(e.g. leaving `font-lock-mode' off for the buffer)."
  (condition-case err
      (dd/py-workon-project-venv nil)
    (error (message "btp/py-setup-venv: %s" (error-message-string err)))))

(add-hook 'python-base-mode-hook #'btp/py-setup-venv)

(defun btp/py-auto-eglot ()
  "Activate the project virtual environment and restart Eglot.
Prompt for an environment when none matches the project name."
  (interactive)
  (unless (dd/py-workon-project-venv t)
    (call-interactively #'pyvenv-workon))
  (if-let* ((server (eglot-current-server)))
      (eglot-reconnect server t)
    (eglot-ensure)))

(use-package flymake-ruff
  :ensure t
  :commands flymake-ruff-load
  :hook
  ((python-base-mode . flymake-ruff-load)
   (eglot-managed-mode . flymake-ruff-load)))

(use-package eglot
  :ensure nil
  :config
  ;; Register ty for all built-in Python modes.
  (add-to-list 'eglot-server-programs
               '((python-base-mode :language-id "python") . ("ty" "server")))
  :hook (python-base-mode . eglot-ensure))
