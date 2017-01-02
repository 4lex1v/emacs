(eval-and-compile
  (let ((foundation-packages '("use-package" "async"))
        (folder               (concat user-emacs-directory "foundation/")))
    (mapc #'(lambda (pkg) (add-to-list 'load-path (expand-file-name pkg folder)))
          foundation-packages)))

;; `use-package' configuration
(setq use-package-verbose               t
      use-package-enable-imenu-support  t
      use-package-check-before-init     t
      use-package-minimum-reported-time 0.01)

(require 'use-package)

(use-package async
  :load-path "core/async"
  :config (use-package async-bytecomp))
