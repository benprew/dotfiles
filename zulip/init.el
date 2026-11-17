;;; -*- lexical-binding: t; -*-

;;; Zulip uses mypy for type checking, use that for zulip projects

(require 'flymake)

(defun btp/zulip-root ()
  "Return the configured Zulip checkout root for the current file."
  (let ((root (file-name-as-directory
               (file-truename (expand-file-name "~/src/zulip/zulip")))))
    (when (and buffer-file-name (file-in-directory-p buffer-file-name root))
      root)))

(defun btp/zulip-python-buffer-p ()
  "Return non-nil for Python buffers in the Zulip checkout."
  (and (derived-mode-p 'python-base-mode) (btp/zulip-root)))

(defun btp/zulip-eglot-ensure (original &rest args)
  "Allow automatic Eglot startup outside Zulip Python buffers."
  (unless (btp/zulip-python-buffer-p)
    (apply original args)))

(defun btp/zulip-auto-eglot (original &rest args)
  "Keep the Python Eglot shortcut from starting ty in Zulip."
  (if (btp/zulip-python-buffer-p)
      (user-error "Zulip uses mypy and Ruff; use M-x zulip-lint for the full suite")
    (apply original args)))

(with-eval-after-load 'eglot
  (advice-add 'eglot-ensure :around #'btp/zulip-eglot-ensure))
(advice-add 'btp/py-auto-eglot :around #'btp/zulip-auto-eglot)

(defun btp/zulip-disable-standalone-checker ()
  "Remove the redundant checker before Flymake starts in Zulip Python buffers."
  (when (btp/zulip-python-buffer-p)
    (remove-hook 'flymake-diagnostic-functions #'python-flymake t)))

(add-hook 'prog-mode-hook #'btp/zulip-disable-standalone-checker -10)

(defvar-local btp/zulip-mypy-process nil)

(defun btp/zulip-mypy (report-fn &rest _args)
  "Check the saved file with the mypy wrapper used by tools/lint."
  (when (process-live-p btp/zulip-mypy-process)
    (delete-process btp/zulip-mypy-process))
  (if (or (buffer-modified-p) (not buffer-file-name))
      (funcall report-fn nil)
    (let* ((source (current-buffer))
           (root (btp/zulip-root))
           (default-directory root)
           (process-environment (copy-sequence process-environment))
           (python (expand-file-name ".venv/bin/python" root))
           (output (generate-new-buffer " *zulip-mypy*")))
      (setenv "VIRTUAL_ENV" (expand-file-name ".venv" root))
      (setenv "PATH" (concat (expand-file-name ".venv/bin" root)
                             path-separator (getenv "PATH")))
      (setq btp/zulip-mypy-process
            (make-process
             :name "zulip-mypy" :buffer output :noquery t
             :command (list python "tools/run-mypy" "--quiet"
                            (file-relative-name buffer-file-name root))
             :sentinel
             (lambda (proc _event)
               (when (memq (process-status proc) '(exit signal))
                 (unwind-protect
                     (when (buffer-live-p source)
                       (with-current-buffer source
                         (when (and (eq proc btp/zulip-mypy-process)
                                    (eq (process-status proc) 'exit)
                                    (not (buffer-modified-p)))
                           (let (diags)
                             (with-current-buffer output
                               (goto-char (point-min))
                               (while (re-search-forward
                                       "^\\(.+\\):\\([0-9]+\\):\\(?:[0-9]+:\\)? \\(error\\|warning\\|note\\): \\(.*\\)$" nil t)
                                 (when (file-equal-p
                                        (expand-file-name (match-string 1) root)
                                        (buffer-file-name source))
                                   (let* ((line (string-to-number (match-string 2)))
                                          (kind (intern (concat ":" (match-string 3))))
                                          (message (match-string 4))
                                          (region (flymake-diag-region source line)))
                                     (when region
                                       (push (flymake-make-diagnostic
                                              source (car region) (cdr region)
                                              kind message) diags))))))
                             (if (and (> (process-exit-status proc) 1) (not diags))
                                 (funcall report-fn :panic :explanation
                                          (with-current-buffer output (buffer-string)))
                               (funcall report-fn diags))))))
                   (kill-buffer output)))))))))

(defun btp/zulip-python-diagnostics ()
  "Use Zulip's mypy configuration and virtual environment in its checkout."
  (when-let* ((root (btp/zulip-root)))
    (setq-local flymake-ruff-program (expand-file-name ".venv/bin/ruff" root))
    (add-hook 'flymake-diagnostic-functions #'btp/zulip-mypy nil t)
    (add-hook 'after-save-hook #'flymake-start nil t)))

(add-hook 'python-base-mode-hook #'btp/zulip-python-diagnostics)

(defun zulip-lint ()
  "Run the full tools/lint suite using Zulip's virtual environment."
  (interactive)
  (let* ((default-directory (or (btp/zulip-root)
                                (user-error "Not in the Zulip checkout")))
         (process-environment (copy-sequence process-environment)))
    (setenv "VIRTUAL_ENV" (expand-file-name ".venv"))
    (setenv "PATH" (concat (expand-file-name ".venv/bin") path-separator (getenv "PATH")))
    (compile ".venv/bin/python tools/lint")))

(defun zulip-test-at-point ()
  (interactive)
  (let* ((root (project-root (project-current t)))
         (file (file-relative-name (buffer-file-name) root))
         (module (replace-regexp-in-string
                  "/" "."
                  (file-name-sans-extension file)))
         (defun (python-info-current-defun))
         (python (expand-file-name ".venv/bin/python" root)))
    (unless defun
      (user-error "Point is not inside a Python definition"))
    (let ((default-directory root))
      (compile
       (format "%s tools/test-backend %s.%s"
               (shell-quote-argument python)
               module
               defun)))))


;; Install/package-install `ruff-format` first.

(use-package ruff-format
  :ensure t
  :hook (python-base-mode . ruff-format-on-save-mode))
