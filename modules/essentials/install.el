(defsubst 4lex1v/hook-into-modes (func &rest modes)
  (dolist (mode-hook modes) (add-hook mode-hook func)))

(defsubst 4lex1v/mode-hooks (mode &rest funcs)

  (declare (indent 1))
  (dolist (f funcs)
    (add-hook mode f)))

(defsubst if-bound-f (f &optional args)
  "Helper function to guard against unbound functions"
  (if (fboundp ',f) (funcall ',f args)))

(defmacro func (name &rest body)
  "Shortcut for basic interactive no-arg functions"
  `(defun ,name ()
     (interactive)
     ,@body))

(defmacro -func (body)

  `(lambda ()
     (interactive)
     ,@body))

(defmacro with-package (pkg-name &rest body)
  (declare (indent 1))
  `(if (fboundp ',pkg-name) (progn ,@body)))

(defmacro with-mode (mode-name &rest body)
  (declare (indent 1))
  (let ((mode-symb (intern (format "%s-mode" mode-name))))
    `(if (fboundp ',mode-symb) (progn ,@body))))

(defun string/starts-with (string prefix)
  "Return t if STRING starts with prefix."
  (and (string-match (rx-to-string `(: bos ,prefix) t)
                     string)
       t))

(setq-default truncate-lines t
              initial-major-mode (quote fundamental-mode))

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
      desktop-save-mode           nil
      desktop-save                nil
      user-ref-name               "4lex1v"
      mouse-wheel-scroll-amount   '(1)
      mouse-wheel-progressive-speed nil)

(if (file-exists-p "~/Sandbox")
    (setq default-directory "~/Sandbox/"))

(require 'package)

(setq package-enable-at-startup nil
      package--init-file-ensured t
      package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

;; https://www.reddit.com/r/emacs/comments/53zpv9/how_do_i_get_emacs_to_stop_adding_custom_fields/
(defun package--save-selected-packages (&rest opt) nil)

(package-initialize)

(eval-when-compile 
  ;; `use-package' configuration
  (setq use-package-verbose               t
        use-package-always-defer          t
        use-package-enable-imenu-support  t
        use-package-check-before-init     t
        use-package-minimum-reported-time 0.1)
  
  ;; Only when the config is stable
  (setq use-package-expand-minimally t)

  (require 'use-package-core)
  (require 'use-package)

  (use-package general :demand t
    :init
    (setq general-default-states  'normal
          general-default-prefix  "<SPC>")
    :config
    (general-evil-setup t))

  ;; Use-Package Extensions
  (use-package upe-hooks :demand t))

;; Vendor packages
(use-package diminish :ensure t)
(use-package async :ensure t :pin "melpa")

(unbind-key "C-x b")

(use-package mode-local :demand t)

(use-package helpful :ensure t
  :after elisp-refs)





