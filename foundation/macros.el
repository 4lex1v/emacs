(defmacro with-package (pkg-name &rest body)
  (declare (indent 1))
  `(if (fboundp ',pkg-name) (progn ,@body)))

(defmacro with-mode (mode-name &rest body)
  (declare (indent 1))
  (let ((mode-symb (intern (format "%s-mode" mode-name))))
    `(if (fboundp ',mode-symb) (progn ,@body))))
