;; for nand2tetris
;; (add-to-list 'auto-mode-alist '("\\.hdl$" . vhdl-mode))
;; (add-hook 'asm-mode-hook (lambda () (whitespace-mode -1)))

(require 'load-relative)
(load-relative "./init-nand2tetris.el")

(require 'init-nand2tetris)


(add-hook 'jack-mode-hook
          (set (make-local-variable 'compile-command) "JackCompiler.sh"))

(global-set-key "\C-cm" 'compile)
