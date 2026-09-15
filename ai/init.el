;;; -*- lexical-binding: t; -*-
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
