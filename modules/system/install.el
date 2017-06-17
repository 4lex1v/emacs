(use-package exec-path-from-shell
  :if (or (eq system-type 'darwin)
          (eq system-type 'gnu/linux))
  :commands exec-path-from-shell-getenv
  :init
  (let* ((brew-bin-path "/usr/local/homebrew/bin")
         (usr-local-bin "/usr/local/bin")
         (current-path  (getenv "PATH"))
         (new-path      (format "%s:%s:%s" brew-bin-path usr-local-bin current-path)))

    ;; To find apps, e.g SBT
    (add-to-list 'exec-path brew-bin-path)
    (add-to-list 'exec-path usr-local-bin)
    
    ;; For Eshell
    (setenv "PATH" new-path)))

(use-package osx
  :if (mac-os-p)
  :init
  (setq browse-url-browser-function 'browse-url-default-macosx-browser
        delete-by-moving-to-trash    t
        mac-command-modifier        'meta
        mac-option-modifier         'super
        mac-control-modifier        'control
        ns-function-modifier        'hyper
        ns-use-native-fullscreen     t
        frame-resize-pixelwise       t))


(use-package term
  :config
  (add-hook 'term-load-hook 'term-line-mode))

