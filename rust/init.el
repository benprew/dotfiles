;;; -*- lexical-binding: t; -*-

(use-package rust-mode
  :ensure t)

(defun own/rust-use-eglot-diagnostics ()
  "Leave Rust tree-sitter diagnostics to Eglot and rust-analyzer."
  (when (derived-mode-p 'rust-ts-mode)
    (remove-hook 'flymake-diagnostic-functions #'rust-ts-flymake t)))

;; Run before the global prog-mode Flymake hook.  Otherwise the standalone
;; checker can report after Eglot takes over and clears its Flymake state.
(add-hook 'prog-mode-hook #'own/rust-use-eglot-diagnostics -10)

;; GUI Emacs may not inherit the environment from the shell init files.
;; Prefer system tools on Fedora, retaining Cargo-installed utilities as fallback.
(let* ((cargo-bin (expand-file-name "bin" (or (getenv "CARGO_HOME") "~/.cargo")))
       (paths (delete cargo-bin (split-string (or (getenv "PATH") "") path-separator t)))
       (system-rust (file-exists-p "/etc/fedora-release")))
  (setq exec-path (delete cargo-bin exec-path))
  (if system-rust
      (setq exec-path (append exec-path (list cargo-bin))
            paths (append paths (list cargo-bin)))
    (push cargo-bin exec-path)
    (push cargo-bin paths))
  (setenv "PATH" (mapconcat #'identity paths path-separator)))

(use-package eglot
  :ensure nil
  :config
  ;; Use Clippy's normal lint levels on save; projects can override this via
  ;; eglot-workspace-configuration in .dir-locals.el.
  (let* ((workspace-config (copy-tree (default-value 'eglot-workspace-configuration)))
         (config (plist-get workspace-config :rust-analyzer))
         (check (plist-get config :check)))
    (setq config (plist-put config :check (plist-put check :command "clippy")))
    (setq config (plist-put config :checkOnSave t))
    (setq-default eglot-workspace-configuration
                  (plist-put workspace-config :rust-analyzer config)))
  :hook ((rust-mode rust-ts-mode) . eglot-ensure))

(defun own/rust-format-on-save ()
  "Format Rust buffers when Eglot provides formatting."
  (when (and (eglot-managed-p)
             (eglot-server-capable :documentFormattingProvider))
    (eglot-format-buffer)))

(defun own/setup-rust-format-on-save ()
  "Install a buffer-local Rust formatting hook."
  (add-hook 'before-save-hook #'own/rust-format-on-save nil t))

(dolist (hook '(rust-mode-hook rust-ts-mode-hook))
  (add-hook hook #'own/setup-rust-format-on-save))

(when (and (fboundp 'rust-ts-mode)
           (fboundp 'treesit-language-available-p)
           (treesit-language-available-p 'rust))
  (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode)))
