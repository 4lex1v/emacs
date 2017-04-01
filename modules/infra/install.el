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

; (use-package eshell
;   :init (require 'em-alias)
;   :config
;   (defun 4lex1v:helm-eshell-history ()
;     (eshell-cmpl-initialize)
;     (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
;     (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history))

;   (add-hook 'eshell-mode-hook #'4lex1v:helm-eshell-history)
;   (add-hook 'eshell-mode-hook #'ansi-color-for-comint-mode-on)
  
;   ;; MacOS specific aliases
;   (if (eq system-type 'darwin)
;       (eshell/alias "bubu" "brew update && brew upgrade")))
