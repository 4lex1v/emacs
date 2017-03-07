(use-package docker
  :load-path "modules/infra/docker"
  :commands docker-images

  :init (setq default-docker-machine-name "universe")
  
  :config (progn
            (defun docker-machine-connect (name)
              "Connects to a running machine by its `name'"
              (interactive "sDocker-machine name: ")
              (let ((docker-env
                     (mapcar
                      #'(lambda (line)
                          (split-string-and-unquote
                           (replace-regexp-in-string "\\(export \\|=\\)" " " line)))
                      (-filter
                       #'(lambda (line) (string/starts-with line "export"))
                       (split-string
                        (shell-command-to-string
                         (concat "docker-machine env " name)) "\n" t)))))
                (mapc
                 #'(lambda (params) (apply 'setenv params))
                 docker-env)))

            (if (eq (shell-command
                     (concat "docker-machine env "
                             default-docker-machine-name))
                    0)
                (docker-machine-connect default-docker-machine-name))))

(use-package eshell
  :init
  (defun 4lex1v:helm-eshell-history ()
    (eshell-cmpl-initialize)
    (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
    (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history))

  (add-hook 'eshell-mode-hook #'4lex1v:helm-eshell-history)
  (add-hook 'eshell-mode-hook #'ansi-color-for-comint-mode-on)

  :config
  (require 'em-alias)
  
  ;; MacOS specific aliases
  (if (eq system-type 'darwin)
      (eshell/alias "bubu" "brew update && brew upgrade")))
