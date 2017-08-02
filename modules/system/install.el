
(defconst IS_MAC     (eq system-type 'darwin))
(defconst IS_LINUX   (eq system-type 'gnu/linux))
(defconst IS_WINDOWS (eq system-type 'windows-nt))

(use-package exec-path-from-shell
  :if (or IS_MAC IS_LINUX)
  :commands exec-path-from-shell-getenv
  :init
  
  ;; TODO :: Check if it works on Windows
  (defun register-path-folders (&rest paths)
    (declare (indent 1))
    (let ((path (-reduce-r-from
                 (lambda (value acc) (format "%s:%s" value acc))
                 (exec-path-from-shell-getenv "PATH")
                 paths)))
     (exec-path-from-shell-setenv "PATH" path))))

(use-package osx
  :if IS_MAC
  :init
  (setq browse-url-browser-function 'browse-url-default-macosx-browser
        delete-by-moving-to-trash    t
        mac-command-modifier        'meta
        mac-option-modifier         'super
        mac-control-modifier        'control
        ns-function-modifier        'hyper
        ns-use-native-fullscreen     t
        frame-resize-pixelwise       t)
  :config
  (register-path-folders "/usr/local/homebrew/bin" "/usr/local/bin")
  (message "Current path :: %s" (getenv "PATH"))

  (use-package em-alias
    :config
    (eshell/alias "bubu" "brew update && brew upgrade"))
  
  )

(use-package term
  :config
  (add-hook 'term-load-hook 'term-line-mode))

