(defmacro with-package (pkg-name &rest body)
  (declare (indent 1))
  `(unless (fboundp ',pkg-name) (progn ,@body)))

(defmacro with-mode (mode-name &rest body)
  (declare (indent 1))
  (let ((mode-symb (make-symbol (format "%s-mode" mode-name))))
    `(unless (fboundp ',mode-symb) (progn ,@body))))

