;; General Emacs configuration

(defun 4lex1v/no-backup-and-autosave ()
  (setq make-backup-files nil)
  (setq auto-save-default nil))

(defun 4lex1v/quite-start ()
  (setq inhibit-startup-message t)
  (setq initial-scratch-message nil))

(defun 4lex1v/enable-reopen-last-session ()
	(setq desktop-save t) ;; always save, never ask permition
	(desktop-save-mode 1))

(defun 4lex1v/enable-narrowing ()
	(put 'narrow-to-region 'disabled nil)
	(put 'narrow-to-page 'disabled nil))

(defun 4lex1v/configure-scratch ()
	(setq initial-major-mode 'scala-mode))	
	
;;;;;;;;;;;;;;;;;
(4lex1v/quite-start)
(4lex1v/no-backup-and-autosave)
(4lex1v/enable-reopen-last-session)
(4lex1v/enable-narrowing)
(4lex1v/configure-scratch)

(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default major-mode 'org-mode)
							
(setq tramp-default-method "ssh")
