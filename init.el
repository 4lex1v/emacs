(require 'seq)

(setq
 gc-cons-threshold       (* 100 1024 1024)  ;; 100MB
 read-process-output-max (*   1 1024 1024)) ;;   1MB

(defconst USER-EMACS-DIRECTORY (file-name-directory (or load-file-name (buffer-file-name))))
(defconst USER-INIT-FILE       (concat USER-EMACS-DIRECTORY "init.el"))

(defconst IS-MAC               (eq system-type 'darwin))
(defconst IS-WINDOWS           (eq system-type 'windows-nt))
(defconst IS-UNIX              (not IS-WINDOWS))

(let ((customs '("fixes.el" "__private.el" "defs.el")))
  (mapc
   (lambda (filename)
     (let ((file-path (concat USER-EMACS-DIRECTORY filename)))
       (if (file-exists-p file-path)
           (load-file file-path))))
   customs))

(defvar god-mode-map (make-sparse-keymap))

(defun enable-and-configure-god-mode ()
  (setq god-mode-enabled t)
  (setq cursor-type 'box))

(defun turn-off-god-mode ()
  (setq god-mode-enabled nil)
  (setq cursor-type 'bar))

(defun 4l/project-rebuild ()
  (interactive)
  (let ((default-directory (cdr (project-current t))))
    (call-interactively 'recompile)))

(let ((font-setting (if IS-WINDOWS "Iosevka SS08 Slab Extended-14" "Iosevka Light-20")))
  (add-to-list 'initial-frame-alist (cons 'font font-setting))
  (setq default-frame-alist initial-frame-alist)
  (set-frame-font font-setting))

(add-to-list 'load-path (expand-file-name "themes/sirthias" USER-EMACS-DIRECTORY))
(when (locate-library "sirthias-theme")
  (setq sirthias-easy-mode t)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (select-frame frame)
              (load-theme 'sirthias t)))
  (require 'sirthias-theme nil nil)
  (load-theme 'sirthias t))

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(fset 'yes-or-no-p   'y-or-n-p)

(show-paren-mode       t)
(delete-selection-mode t)
(global-auto-revert-mode t)
(prefer-coding-system 'utf-8-unix)
(column-number-mode 1)
(recentf-mode 1)
(blink-cursor-mode 0)
(tool-bar-mode -1)

(setq-default
 auto-window-vscroll nil
 truncate-lines      nil
 initial-major-mode (quote fundamental-mode)
 mode-line-default-help-echo nil
 tab-width           2
 indent-tabs-mode    nil
 cursor-type        'box
 cursor-in-non-selected-windows 'bar
 frame-title-format "%f"
 linum-format       "%3d " 
 load-prefer-newer  t
 left-fringe-width  20
 word-wrap t
 line-spacing 2

 buffer-file-coding-system 'utf-8-unix

 case-fold-search nil
 case-replace     nil)

(setq
 inhibit-startup-message        t
 initial-scratch-message        nil
 initial-buffer-choice          nil
 show-paren-delay               0.0
 ring-bell-function            'ignore
 tramp-default-method          "ssh"
 make-backup-files              nil
 auto-save-default              nil
 kill-do-not-save-duplicates    t
 ad-redefinition-action        'accept
 next-line-add-newlines         nil
 desktop-save-mode              nil
 desktop-save                   nil
 default-directory             "~/"
 user-ref-name                 "4lex1v"
 mouse-wheel-scroll-amount     '(1)
 mouse-wheel-progressive-speed  nil
 history-delete-duplicates      t
 custom-file                   (concat USER-EMACS-DIRECTORY "custom.el")

 ;; Performance related settings
 inhibit-compacting-font-caches t
 jit-lock-defer-time 0
 fast-but-imprecise-scrolling t
 search-upper-case              nil
 dabbrev-case-distinction nil
 scroll-step 5) ;; optimal balance between scroll-speed and line flickering

(if IS-MAC
    (progn
      (setq
       browse-url-browser-function 'browse-url-default-macosx-browser
       delete-by-moving-to-trash    t
       mac-command-modifier        'meta
       mac-option-modifier         'super
       mac-control-modifier        'control
       ns-function-modifier        'hyper
       ns-use-native-fullscreen     t
       frame-resize-pixelwise       t)))

(if IS-WINDOWS
    (progn
      (require 'subr-x)
      (if-let ((powershell-exe-path (executable-find "pwsh.exe")))
          (setq shell-file-name powershell-exe-path))))

;; To avoid acidental hits on the touchpad
(dolist (binding '("<mouse-1>" "<C-mouse-1>" "<C-down-mouse-1>"))
  (define-key global-map (kbd binding)
    (lambda nil (interactive) nil)))

(setq
 dabbrev-case-replace t
 dabbrev-case-fold-search t
 dabbrev-upcase-means-case-search t)

(abbrev-mode 1)

(when (and (require 'ls-lisp) (require 'dired))
  (setq
   ls-lisp-dirs-first t
   ls-lisp-use-insert-directory-program nil))

(setq
 package-enable-at-startup nil
 package-check-signature nil
 package--init-file-ensured t
 package-archives '(("melpa-stable" . "http://stable.melpa.org/packages/")
                    ("gnu"          . "http://elpa.gnu.org/packages/")))

;; https://www.reddit.com/r/emacs/comments/53zpv9/how_do_i_get_emacs_to_stop_adding_custom_fields/
(defun package--save-selected-packages (&rest opt) nil)
(require 'package) 
(package-initialize)

(package-install 'use-package)

(setq
 use-package-verbose               t
 use-package-always-defer          t
 use-package-enable-imenu-support  t
 use-package-check-before-init     t
 use-package-minimum-reported-time 0.1

 ;; Only when the config is stable
 use-package-expand-minimally t)

(use-package diminish :ensure t)
(use-package s        :ensure t :demand t) ;; Strings manipulation library
(use-package f        :ensure t :demand t) ;; Files manipulation library
(use-package dash     :ensure t :demand t) ;; List manipulation library

(use-package exec-path-from-shell :ensure t :demand t
  :commands (exec-path-from-shell-getenv
             exec-path-from-shell-setenv)
  :init
  ;; Under certain conditions this can be nicely used withing Windows environment as well...
  (defun run-shell-command (&rest cmd)
    (replace-regexp-in-string "\r?\n\\'" ""
                              (shell-command-to-string
                               (mapconcat 'identity cmd " ")))) 
  
  (defun register-path-folders (&rest paths)
    (declare (indent 1))
    (let ((path (-reduce-r-from
                 (lambda (value acc) (format "%s:%s" value acc))
                 (exec-path-from-shell-getenv "PATH")
                 paths)))
      (exec-path-from-shell-setenv "PATH" path)))

  :config
  (if IS-MAC
      (progn
        (exec-path-from-shell-setenv "HOMEBREW_PREFIX" "/usr/local")
        (exec-path-from-shell-setenv "HOMEBREW_CELLAR" "/usr/local/Cellar")
        (exec-path-from-shell-setenv "GTAGSCONF" "/usr/local/share/gtags/gtags.conf")
        (exec-path-from-shell-setenv "GTAGSLABEL" "ctags")
        (register-path-folders "/usr/local/opt/llvm/bin" "/usr/local/homebrew/bin" "/usr/local/bin")))

  (if IS-WINDOWS
      (progn
        (exec-path-from-shell-setenv "SHELL" "c:/Users/Aleksandr/scoop/apps/pwsh/current/pwsh.exe"))))

;;(load-file (concat USER-EMACS-DIRECTORY "classic_config.el"))
(load-file (concat USER-EMACS-DIRECTORY "evil_config.el"))

(mapc
 (lambda (mode)
   (font-lock-add-keywords ;;`font-lock-keywords`
    mode
    '(("#\\<\\(TODO\\)\\>" 1 '(error :underline t) t)
      ("#\\<\\(NOTE\\)\\>" 1 '(warning :underline t) t))))
 '(prog-mode))


