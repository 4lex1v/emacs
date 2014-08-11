;; General Emacs configuration

(defun 4lex1v/no-backup-and-autosave ()
  (setq make-backup-files nil)
  (setq auto-save-default nil))

(defun 4lex1v/quite-start ()
  (setq inhibit-startup-message t)
  (setq initial-scratch-message nil))

;;;;;;;;;;;;;;;;;
(4lex1v/quite-start)
(4lex1v/no-backup-and-autosave)

(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)
