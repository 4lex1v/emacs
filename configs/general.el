;; General Emacs configuration

(require 'ansi-color)

;; User credentials
(setq user-full-name    "Aleksandr Ivanov"
      user-mail-address "4lex1v@gmail.com")

;; Turn on restricted operations
(put  'narrow-to-region        'disabled nil)
(put  'narrow-to-page          'disabled nil)

;;(desktop-save-mode              1)
(fset 'yes-or-no-p             'y-or-n-p)

(setq-default indent-tabs-mode  nil
              gc-cons-threshold 10000000)

(setq ring-bell-function         'ignore
      initial-major-mode         'text-mode
      tramp-default-method       "ssh"
      make-backup-files           nil
      auto-save-default           nil
      inhibit-startup-message     t
      initial-scratch-message     nil
;;      desktop-save                t
      kill-do-not-save-duplicates t
      ad-redefinition-action     'accept
      next-line-add-newlines      t)

(provide 'general)
