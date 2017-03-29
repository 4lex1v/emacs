;; TODO :: remove later
(add-to-list 'load-path (expand-file-name "."))

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
    
    ;; For Eshell
    (setenv "PATH" new-path)))

(use-package osx
  :if (eq system-type 'darwin)
  :init
  (setq browse-url-browser-function 'browse-url-default-macosx-browser
        delete-by-moving-to-trash    t
        mac-command-modifier        'meta
        mac-option-modifier         'super
        mac-control-modifier        'control
        ns-function-modifier        'hyper))
