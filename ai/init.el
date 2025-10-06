(use-package gptel
  :vc (:url "https://github.com/karthink/gptel" :branch "master")
  :ensure t
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
