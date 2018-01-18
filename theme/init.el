(prelude-require-packages '(color-theme-solarized))

(defun night ()
  (interactive)
  (color-theme-solarized-dark))

(defun day ()
  (interactive)
  (color-theme-solarized-light))

(defun theme-init ()
  (day))

(theme-init)
