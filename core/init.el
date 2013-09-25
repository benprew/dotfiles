(require-el-get 'undo-tree)
(require-el-get 'evil)
(require-el-get 'helm)
(require-el-get 'workgroups)
(require-el-get '(:name flycheck
                         :type elpa))
(require-el-get '(:name ag
       :description "Ag plugin for emacs"
       :type github
       :pkgname "Wilfred/ag.el"))

(require 'cl)

(defalias 'qrr 'query-replace-regexp)
(defalias 'sir 'string-insert-rectangle)

(defun duration (time)
  "Takes in a time-value and returns the number of seconds since
   the epoch that value represents."
  (+ (* 65536 (car time)) (cadr time)))

(defun uptime ()
  "Prints the current uptime of Emacs as recorded on startup in
   the value 'start-time'"
  (interactive)
  (let* ((total (duration (time-subtract (current-time) start-time)))
         (days  (floor (/ total 86400)))
         (hours (progn (decf total (* days  86400)) (floor (/ total 3600))))
         (mins  (progn (decf total (* hours 3600))  (floor (/ total 60))))
         (secs  (progn (decf total (* mins  60))    (floor total))))
    (message "%d days, %d hours, %d minutes, %d seconds" days hours mins secs)))

(defalias 'unfuck-this-buffer 'toggle-input-method)

(defvar prev-frame-height)
(defvar prev-frame-width)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(defun core-init ()
  (require 'pcomplete)
  (require 'uniquify)
  (require 'ibuffer)
  (require 'workgroups)
  (require 'helm-config)

  (setq loaded-init-module t)

  (put 'upcase-region 'disabled nil)
  (setq x-select-enable-clipboard t)
  (put 'narrow-to-region 'disabled nil)
  (windmove-default-keybindings 'meta)

  ;; ido mode
  ;; http://www.masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-auto-merge-work-directories-length nil
        ido-create-new-buffer 'always
        ido-max-prospects 10)

  (setq uniquify-buffer-name-style 'post-forward)

  ;; ibuffer
  ;; http://martinowen.net/blog/2010/02/tips-for-emacs-ibuffer.html
  (setq ibuffer-enable t)
  (setq ibuffer-shrink-to-minimum-size t)
  (setq ibuffer-expert t)
  (global-set-key "\C-x\C-b" 'ibuffer)

  ;; text mode editing
  ;; aspell for ispell http://emacswiki.org/emacs/InteractiveSpell#toc5
  (setq ispell-program-name "/usr/local/bin/aspell")
  (setq ispell-extra-args '("--sug-mode=ultra"))
  (setq ispell-list-command "list")

  (setq initial-scratch-message ";Don't ignore your dreams\n;Don't work too much\n;Say what you think\n;Cultivate friendships\n;Be happy.\n\n")

  (eval-after-load "ispell"
    '(when (executable-find ispell-program-name)
       (add-hook 'text-mode-hook 'turn-on-flyspell)))

  ;; diff coloring
  (eval-after-load 'diff-mode
    '(progn
       (set-face-foreground 'diff-added "green4")
       (set-face-foreground 'diff-removed "red2")))

  (eval-after-load 'magit
    '(progn
       (set-face-foreground 'magit-diff-add "green4")
       (set-face-foreground 'magit-diff-del "red2")))

  ;; emacs lock
  (load "./emacs-lock+")
  (with-current-buffer "*scratch*"
    (setq emacs-lock-from-exiting 1))
  (global-set-key "\C-cu" 'toggle-emacs-lock)


  (global-set-key "\C-cfp" 'find-file-at-point)
  (global-set-key "\C-co" 'multi-occur)
  (global-set-key "\M-/" 'hippie-expand)

  ;; flycheck
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (global-set-key "\C-cen" 'flycheck-next-error)
  (global-set-key "\C-cep" 'flycheck-prev-error)

  ;; Search

  ;;;; ctags
  ;; generation of file is handled via githook: http://blog.tobiascrawley.net/2009/01/01/generating-a-tags-file-from-a-git-hook/
  ;; setup default git template dir
  ;; http://stackoverflow.com/questions/2293498/git-commit-hooks-global-settings

  ;;;; helm
  (recentf-mode 1)
  (setq helm-idle-delay 0.1)
  (setq helm-input-idle-delay 0.1)
  (setq helm-c-locate-command "locate -d ~/src/locate.db %.0s %s")
  (global-set-key "\C-ch" 'helm-for-files)

  ;;;; Ag (like ack, but faster)
  (require 'ag)
  (global-set-key "\C-cg" 'ag-project-at-point)
  (global-set-key "\C-cG" 'ag-project-regexp))

(pre-init (lambda()
  (setq start-time (current-time)) ; for M-x uptime
  (setq visual-bell t)
  (setq ns-command-modifier 'meta) ; this is *super important*
  (setenv "DOTFILESROOT" (concat (getenv "HOME") "/dotfiles/"))
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
))

(post-init 'core-init)

;;; init.el ends here
