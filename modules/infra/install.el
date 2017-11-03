(use-package docker
  :diminish docker-mode
  :defer
  :config
  (docker-global-mode 1))

(use-package docker-images
  :after docker
  :config
  (evil-set-initial-state #'docker-images-mode 'emacs))

(use-package docker-containers
  :after docker
  :config
  (evil-set-initial-state #'docker-containers-mode 'emacs))

(use-package docker-machine
  :after docker
  :config
  (evil-set-initial-state #'docker-machine-mode 'emacs))

(use-package eshell :defer t
  :init
  (defun 4lex1v:helm-eshell-history ()
    (eshell-cmpl-initialize)
    (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
    (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history))

  (defun git-prompt-branch-name ()
    "Get current git branch name"
    (let ((args '("symbolic-ref" "HEAD" "--short")))
      (with-temp-buffer
        (apply #'process-file "git" nil (list t nil) nil args)
        (unless (bobp)
          (goto-char (point-min))
          (buffer-substring-no-properties (point) (line-end-position))))))
  
  (defun 4lex1v:eshell-prompt ()
    (let ((branch-name (git-prompt-branch-name)))
      (concat
       "\n# " (user-login-name) " in " (abbreviate-file-name (eshell/pwd)) "\n"
       (if branch-name (format "git:(%s) >> " branch-name) ">> ")
       )))
  
  (setq eshell-prompt-function #'4lex1v:eshell-prompt
        eshell-prompt-regexp ".*>>+ ")
  
  :hooks (4lex1v:helm-eshell-history
          ansi-color-for-comint-mode-on)
  
  :config
  (with-eval-after-load "em-term"
    (add-to-list 'eshell-visual-commands "htop")))

