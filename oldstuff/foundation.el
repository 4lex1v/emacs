(eval-and-compile
  (let ((foundation-packages '("use-package" "async" "use-package-ext" "emacs-deferred"))
        (folder               (concat user-emacs-directory "oldstuff/")))
    (mapc #'(lambda (pkg) (add-to-list 'load-path (expand-file-name pkg folder)))
          foundation-packages)))

;; `use-package' configuration
(setq use-package-verbose               t
      use-package-enable-imenu-support  t
      use-package-check-before-init     t
      use-package-minimum-reported-time 0.01)

(require 'use-package)

(use-package async :config (use-package async-bytecomp))

(use-package use-package-ext)

(use-package deferred)

(load (expand-file-name "oldstuff/functions" user-emacs-directory))
(load (expand-file-name "oldstuff/macros" user-emacs-directory))
(load (expand-file-name "oldstuff/configuration" user-emacs-directory))

(provide 'foundation)
