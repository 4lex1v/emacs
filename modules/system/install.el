
(defconst IS_MAC     (eq system-type 'darwin))
(defconst IS_WINDOWS (eq system-type 'windows-nt))
(defconst IS_UNIX    (not IS_WINDOWS))

(use-package exec-path-from-shell :ensure t :demand t
  :commands (exec-path-from-shell-getenv
             exec-path-from-shell-setenv)
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
                 (lambda (value acc) (format "%s:%s" acc value))
                 (exec-path-from-shell-getenv "PATH")
                 paths)))
      
      (exec-path-from-shell-setenv "PATH" path))))

(use-package osx :if IS_MAC :demand t
  :after exec-path-from-shell
  
  :init
  (setq browse-url-browser-function 'browse-url-default-macosx-browser
        delete-by-moving-to-trash    t
        mac-command-modifier        'meta
        mac-option-modifier         'super
        mac-control-modifier        'control
        ns-function-modifier        'hyper
        ns-use-native-fullscreen     t
        frame-resize-pixelwise       t
        shell-file-name              "/bin/sh")
  
  :general
  (:prefix nil
   :states '(normal insert)
   "M-`" 'ns-next-frame)
  
  :config
  (message "[CONFIGURATION] Loading MacOS system configuration")

  (exec-path-from-shell-setenv "HOMEBREW_PREFIX" "/usr/local")
  (exec-path-from-shell-setenv "HOMEBREW_CELLAR" "/usr/local/Cellar")
  (exec-path-from-shell-setenv "GTAGSCONF" "/usr/local/share/gtags/gtags.conf")
  (exec-path-from-shell-setenv "GTAGSLABEL" "ctags")
  (register-path-folders "/usr/local/opt/llvm/bin" "/usr/local/homebrew/bin" "/usr/local/bin")

  (use-package em-alias
    :config
    (eshell/alias "bubu" "brew update && brew upgrade")
    (eshell/alias "sshs" "ssh-add ~/.ssh/github_rsa")))

(use-package windows :if IS_WINDOWS
  :init
  (message "Loading Windows OS system configuration")
  
  ;; #NOTE(4lex1v) :: Not sure if these paths should be defined here or in Native modules configuration
  (setq default-directory (expand-file-name "~/")))

(use-package term
  :config
  (add-hook 'term-load-hook 'term-line-mode))
