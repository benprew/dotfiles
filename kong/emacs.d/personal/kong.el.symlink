;;; kong.el --- Kongregate elisp functions

;;; Commentary:

;;; Code:


; Starts a rails console
;; (defun kong-console ()
;;   (interactive)
;;   (remote-shell-command
;;    "etldev"
;;    "bash -c 'export LC_ALL=en_US.UTF-8 && cd /k/analytics_pipeline && bin/console'"
;;    "*console*"))

; Runs a test
(defun kong-run-test-file (file)
  (interactive "Test File: ")
  (compile (format "ssh etldev 'export LC_ALL=en_US.UTF-8 && cd /k/analytics_pipeline && TEST=%s bundle exec rake test INTEGRATION=1'"
file)))

(defun kong-run-current-test-file ()
  (interactive)
  (kong-run-test-file (kong-current-relative-file-name)))

(defun get-current-test-name ()
  (save-excursion
    (let ((pos)
          (test-name))
      (re-search-backward "def \\(test_[a-zA-Z0-9_-]+\\)")
      (buffer-substring-no-properties (match-beginning 1) (match-end 1)))))

(defun kong-run-test-at-point ()
  (interactive)
  (compile
   (format
    "ssh etldev 'export LC_ALL=en_US.UTF-8 && cd /k/analytics_pipeline && bundle exec rake test INTEGRATION=1 LOGTOSTDOUT=1 TEST=%s TESTOPTS='--name=/%s/ -v''"
    (kong-current-relative-file-name)
    (get-current-test-name))))

(defun kong-current-relative-file-name ()
  (mapconcat
   'identity
   (delq nil (let ((test-p "false"))
               (mapcar
                (lambda (el)
                  (if (equal el "test")
                      (setf test-p "true"))
                  (if (equal test-p "true")
                      el
                    nil))
                (split-string (buffer-file-name) "/"))))
   "/"))


(global-unset-key "\C-ck")
(define-key prelude-mode-map "\C-ck" nil)
(global-set-key "\C-ck." 'kong-run-test-at-point)
(global-set-key "\C-ckt" 'kong-run-current-test-file)

(defun kong-ruby-mode-config ()
  "For use in ruby mode."
  (global-unset-key "\C-ck")
  (local-set-key "\C-ck." 'kong-run-test-at-point)
  (local-set-key "\C-ckt" 'kong-run-current-test-file))

(add-hook 'ruby-mode-hook 'kong-ruby-mode-config)
