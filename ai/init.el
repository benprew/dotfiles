(use-package gptel
  :vc (:url "https://github.com/karthink/gptel" :branch "master")
  :ensure t
  :defer t
  :config
  (setq
   gptel-model 'gemini-2.5-flash-preview-04-17
   gptel-backend (gptel-make-gemini "Gemini"
                   :key (with-temp-buffer
                          (insert-file-contents (expand-file-name "~/secrets/gemini.key"))
                          (string-trim (buffer-string)))
                   :stream t)))

(use-package aidermacs
  :ensure t
  :defer t
  :vc (:url "https://github.com/MatthewZMD/aidermacs" :branch "master")
  :config
  (setenv "ANTHROPIC_API_KEY"
          (with-temp-buffer
            (insert-file-contents (expand-file-name "~/secrets/claude.ai.key"))
            (string-trim (buffer-string))))
  (setenv "OPENAI_API_KEY"
          (with-temp-buffer
            (insert-file-contents (expand-file-name "~/secrets/open.ai.key"))
            (string-trim (buffer-string))))
  (setenv "GEMINI_API_KEY"
          (with-temp-buffer
            (insert-file-contents (expand-file-name "~/secrets/gemini.key"))
            (string-trim (buffer-string))))
  (setenv "AIDER_YES_ALWAYS" "YES")
  (global-set-key (kbd "C-c a") 'aidermacs-transient-menu)
  :custom
  ;; Change multiline input key (default is S-<return>)
  (aidermacs-comint-multiline-newline-key "C-<return>")
  (aidermacs-default-chat-mode 'architect)
  (aidermacs-use-architect-mode t) ;; this is deprecated but still needed for now
  (aidermacs-default-model "gemini/gemini-2.5-flash-lite-preview-06-17")
  (aidermacs-architect-model "gemini/gemini-2.5-flash-lite-preview-06-17")
  (aidermacs-editor-model "anthropic/claude-4-sonnet-20250514"))


(with-eval-after-load 'project
  (define-key project-prefix-map "a" #'aidermacs-run)
  (add-to-list 'project-switch-commands '(aidermacs-run "Aider") t))


;; used to start agcli async with a region
(defun async-shell-command-on-region (start end command)
  "Execute COMMAND asynchronously with region as input.
The region between START and END is passed to COMMAND via stdin.
Output appears in *Async Shell Command* buffer."
  (interactive "r\nsShell command on region (async): ")
  (let ((buf (get-buffer-create "*Async Shell Command*"))
        (proc-name "async-shell-region")
        (region-text (buffer-substring-no-properties start end)))
    ;; Clear previous output
    (with-current-buffer buf
      (erase-buffer))
    ;; Display the output buffer
    (display-buffer buf)
    ;; Start async process
    (let ((proc (start-process-shell-command proc-name buf command)))
      ;; Send region text to process stdin
      (process-send-string proc region-text)
      (process-send-eof proc)
      ;; Set up sentinel for completion
      (set-process-sentinel
       proc
       (lambda (process signal)
         (when (memq (process-status process) '(exit signal))
           (with-current-buffer (process-buffer process)
             (goto-char (point-max))
             (insert (format "\n\nProcess %s %s"
                           (process-name process)
                           (substring signal 0 -1))))))))))
