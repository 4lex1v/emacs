
(defconst IS_MAC     (eq system-type 'darwin))
(defconst IS_LINUX   (eq system-type 'gnu/linux))
(defconst IS_WINDOWS (eq system-type 'windows-nt))

(use-package exec-path-from-shell :ensure t
  :commands exec-path-from-shell-getenv
  
  :init
  ;; Under certain conditions this can be nicely used withing Windows environment as well...
  (defun run-shell-command (&rest cmd)
    (replace-regexp-in-string "\r?\n\\'" ""
                              (shell-command-to-string
                               (mapconcat 'identity cmd " ")))) 
  
  ;; TODO :: Check if it works on Windows
  (defun register-path-folders (&rest paths)
    (declare (indent 1))
    (let ((path (-reduce-r-from
                 (lambda (value acc) (format "%s:%s" value acc))
                 (exec-path-from-shell-getenv "PATH")
                 paths)))
      (exec-path-from-shell-setenv "PATH" path))))

(use-package osx :if IS_MAC
  :init
  (message "Loading MacOS system configuration")

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
    (eshell/alias "bubu" "brew update && brew upgrade")
    (eshell/alias "sshs" "ssh-add ~/.ssh/github_rsa")))

(use-package windows :if IS_WINDOWS
  :init
  (message "Loading Windows OS system configuration")
  
  ;; #NOTE(4lex1v) :: Not sure if these paths should be defined here or in Native modules configuration
  (setq win32-system-include-paths '("c:/Program Files (x86)/Windows Kits/10/Include/10.0.16299.0/shared"
                                     "c:/Program Files (x86)/Windows Kits/10/Include/10.0.16299.0/ucrt"
                                     "c:/Program Files (x86)/Windows Kits/10/Include/10.0.16299.0/um"
                                     "c:/Program Files (x86)/Windows Kits/10/Include/10.0.16299.0/winrt"))
  :config
  )

(use-package term
  :config
  (add-hook 'term-load-hook 'term-line-mode))
