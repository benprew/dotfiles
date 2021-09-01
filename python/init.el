(setq flycheck-python-pycompile-executable "python3")
(setq flycheck-python-flake8-executable "python3")

(require 'use-package)

(use-package python-mode
  :ensure t
  :bind ("C-c C-a" . btp/py-auto-eglot))


(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions"))

(use-package blacken
  :defer t
  :ensure t
  :hook (python-mode . blacken-mode))

(use-package jinja2-mode
  :defer 2
  :ensure t
  :mode "\\.jt'")


(defun dd/py-workon-project-venv ()
  "Call pyenv-workon with the current projectile project name.
This will return the full path of the associated virtual
environment found in $WORKON_HOME, or nil if the environment does
not exist."
  (let ((pname (projectile-project-name)))
    (pyvenv-workon pname)
    (if (file-directory-p pyvenv-virtual-env)
        pyvenv-virtual-env
      (pyvenv-deactivate))))

(defun btp/py-auto-eglot ()
  "Turn on lsp mode in a Python project with some automated logic.
Try to automatically determine which pyenv virtual environment to
activate based on the project name, using
`dd/py-workon-project-venv'. If successful, call `lsp'. If we
cannot determine the virtualenv automatically, first call the
interactive `pyvenv-workon' function before `lsp'"
  (interactive)
  (let ((pvenv (dd/py-workon-project-venv)))
    (if pvenv
        (call-interactively #'eglot-reconnect)
      (progn
        (call-interactively #'pyvenv-workon)
        (call-interactively #'eglot-reconnect)))))
