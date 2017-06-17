(setq-default truncate-lines t)

(setq show-paren-delay            0.0
      ring-bell-function         'ignore
      tramp-default-method       "ssh"
      make-backup-files           nil
      auto-save-default           nil
      inhibit-startup-message     t
      initial-scratch-message     nil
      kill-do-not-save-duplicates t
      ad-redefinition-action     'accept
      next-line-add-newlines      t
      desktop-save-mode           t)

;; https://www.reddit.com/r/emacs/comments/53zpv9/how_do_i_get_emacs_to_stop_adding_custom_fields/
(defun package--save-selected-packages (&rest opt) nil)

