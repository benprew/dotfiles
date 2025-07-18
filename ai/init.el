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

(use-package minuet
  :ensure t
  :bind
  (("M-y" . #'minuet-complete-with-minibuffer) ;; use minibuffer for completion
   ("M-i" . #'minuet-show-suggestion) ;; use overlay for completion
   ("C-c m" . #'minuet-configure-provider)
   :map minuet-active-mode-map
   ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
   ("M-p" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
   ("M-n" . #'minuet-next-suggestion) ;; invoke completion or cycle to previous completion
   ("M-A" . #'minuet-accept-suggestion) ;; accept whole completion
   ;; Accept the first line of completion, or N lines with a numeric-prefix:
   ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
   ("M-a" . #'minuet-accept-suggestion-line)
   ("M-e" . #'minuet-dismiss-suggestion))

  :init
  ;; if you want to enable auto suggestion.
  ;; Note that you can manually invoke completions without enable minuet-auto-suggestion-mode
  (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)

  :config
  ;; You can use M-x minuet-configure-provider to interactively configure provider and model
  (setq minuet-provider 'gemini)
  (setenv "GEMINI_API_KEY"
          (with-temp-buffer
            (insert-file-contents (expand-file-name "~/secrets/gemini.key"))
            (string-trim (buffer-string))))
  (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 64))


(use-package aidermacs
  :ensure t
  :vc (:url "ht
tps://github.com/MatthewZMD/aidermacs" :branch "master")
  :config
  (setq aidermacs-default-chat-mode 'architect)
  (setq aidermacs-use-architect-mode t) ;; this is deprecated but still needed for now
  (setq aidermacs-default-model "gemini/gemini-2.5-flash-lite-preview-06-17")
  (setq aidermacs-architect-model "gemini/gemini-2.5-flash-lite-preview-06-17")
  (setq aidermacs-editor-model "anthropic/claude-4-sonnet-20250514")
  (setenv "ANTHROPIC_API_KEY"
          (with-temp-buffer
            (insert-file-contents (expand-file-name "~/secrets/claude.ai.key"))
            (string-trim (buffer-string))))
  (setenv "GEMINI_API_KEY"
          (with-temp-buffer
            (insert-file-contents (expand-file-name "~/secrets/gemini.key"))
            (string-trim (buffer-string))))
  (setenv "AIDER_YES_ALWAYS" "YES")
  (global-set-key (kbd "C-c a") 'aidermacs-transient-menu))
;; Change multiline input key (default is S-<return>)
(setq aidermacs-comint-multiline-newline-key "C-<return>")
