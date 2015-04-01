;; General Emacs configuration

(defconst configs-dir (expand-file-name (concat user-emacs-directory "configs/")))

(defun 4lex1v/configure-user (name email)
  (setq user-full-name name
        user-mail-address email))

;;;;;;;;;;;;;;;;;
(4lex1v/configure-user         "Alexander Ivanov" "4lex1v@gmail.com")
(desktop-save-mode              1)
(fset 'yes-or-no-p             'y-or-n-p)
(put  'narrow-to-region        'disabled nil)
(put  'narrow-to-page          'disabled nil)

(setq-default indent-tabs-mode  nil
              major-mode       'org-mode)

(setq ring-bell-function         'ignore
      initial-major-mode         'scala-mode
      tramp-default-method       "ssh"
      make-backup-files           nil
      auto-save-default           nil
      inhibit-startup-message     t
      initial-scratch-message     nil
      desktop-save                t
      kill-do-not-save-duplicates t)

(provide 'general)
