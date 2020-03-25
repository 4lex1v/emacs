
(setq gc-cons-threshold (* 50 1024 1024))

(defconst USER-EMACS-DIRECTORY (file-name-directory (or load-file-name (buffer-file-name))))
(defconst USER-INIT-FILE       (concat USER-EMACS-DIRECTORY "init.el"))

(defconst IS-MAC               (eq system-type 'darwin))
(defconst IS-WINDOWS           (eq system-type 'windows-nt))
(defconst IS-UNIX              (not IS-WINDOWS))

;; Frame configuration
(let ((font-setting "Iosevka SS08 Light-16"))
  ;; TODO: create a window in the middle of th screen 
  (add-to-list 'initial-frame-alist (cons 'font font-setting))
  (setq default-frame-alist initial-frame-alist)
  ;; Would reload active fonts on eval
  (set-frame-font font-setting))

(add-to-list 'load-path (expand-file-name "themes/sirthias" USER-EMACS-DIRECTORY))
(when (locate-library "sirthias-theme")
  (setq sirthias-easy-mode t sirthias-cold-mode nil)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (select-frame frame)
              (load-theme 'sirthias t)))
  (require 'sirthias-theme nil nil)
  (load-theme 'sirthias t))

(setq-default
 auto-window-vscroll nil
 truncncate-lines    nil
 initial-major-mode (quote fundamental-mode)
 mode-line-default-help-echo nil ; turn-off tooltips on cursor hover-over
 tab-width           2 ;; Though i'm not using tabs
 indent-tabs-mode    nil
 cursor-type        'box
 cursor-in-non-selected-windows 'bar
 frame-title-format "%f"
 linum-format       "%3d "  ;; Try dynamic?
 load-prefer-newer  t
 left-fringe-width  20
 word-wrap t
 line-spacing 2)

(setq
 show-paren-delay               0.0
 ring-bell-function            'ignore
 tramp-default-method          "ssh"
 make-backup-files              nil
 auto-save-default              nil
 inhibit-startup-message        t
 initial-scratch-message        nil
 kill-do-not-save-duplicates    t
 ad-redefinition-action        'accept
 next-line-add-newlines         nil
 desktop-save-mode              nil
 desktop-save                   nil
 user-ref-name                 "4lex1v"
 mouse-wheel-scroll-amount     '(1)
 mouse-wheel-progressive-speed  nil

 ;; EXPERIMENTAL: Aparently this may help stopping Emacs to shit into my init.el
 ;; Found here - https://emacs.stackexchange.com/questions/55018/init-el-and-trampling-of-custom-set-variables
 custom-file (concat USER-EMACS-DIRECTORY "custom.el")
 
 ;; Performance related settings
 inhibit-compacting-font-caches t
 jit-lock-defer-time 0
 fast-but-imprecise-scrolling t

 default-directory              "~/Sandbox/"
 search-upper-case              nil
 safe-local-variable-values    (quote ((user-ref-name . aivanov)))

 case-fold-search nil
 case-replace     nil

 dabbrev-case-distinction nil)

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
       frame-resize-pixelwise       t)
      (global-set-key (kbd "M-`") #'other-frame)))

(tooltip-mode          -1)
(tool-bar-mode         -1)
(scroll-bar-mode       -1)
(blink-cursor-mode     -1)
(show-paren-mode       -1)
(menu-bar-mode         -1)

(delete-selection-mode t)
(global-auto-revert-mode t)

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(fset 'yes-or-no-p   'y-or-n-p)

(defconst noop (lambda nil (interactive) nil))

;; To avoid acidental hits on the touchpad
(dolist (binding '("<mouse-1>" "<C-mouse-1>" "<C-down-mouse-1>"))
  (define-key global-map (kbd binding) noop))
         
(define-key global-map (kbd "C-x f") 'find-file)

(define-key global-map (kbd "M-n")   'forward-paragraph)
(define-key global-map (kbd "M-p")   'backward-paragraph)
(define-key global-map (kbd "C-M-n") 'forward-page)
(define-key global-map (kbd "C-M-p") 'backward-page)

;; #TODO :: search fails if the buffer is opened and not at the beginning of the buffer
(defun 4l/edit-face-at-point ()
  "Editor face of the active theme at the point."
  (interactive)
  (-if-let* ((face-name (face-at-point))
             (theme-file-buffer (find-library (concat (symbol-name THEME_TO_LOAD) "-theme"))))
      (with-current-buffer theme-file-buffer
        (search-forward (symbol-name face-name)))))

(defun 4l/adjust-frame-size ()
  "Resets current frame width to 120 columns"
  (interactive)
  (if (not window-system)
      (error "Can only adjust a frame-based Emacs instance")
    (let ((desirable-size 120))
      (cl-loop
       for window in (window-list (selected-frame) nil)
       do (adjust-window-trailing-edge window (- desirable-size (window-width)) t)))))

(defun 4l/swap-two-windows ()
  (interactive)
  (if (not (eq (length (window-list)) 2))
      (error "Can swap only two windows for now...")
    (let* ((windows-l (window-list))
           (left-window (car windows-l))
           (lw-buffer (window-buffer left-window))
           (right-window (cadr windows-l))
           (rw-buffer (window-buffer right-window)))
      (set-window-buffer left-window rw-buffer)
      (set-window-buffer right-window lw-buffer))))

(define-key global-map (kbd "C-,") #'4l/swap-two-windows)

(defun 4l/insert-block (end-with-semicolon-p)
  (interactive "P")
  (insert "{")
  (newline-and-indent)
  (newline)
  (insert (if end-with-semicolon-p "};" "}"))
  (beginning-of-line)
  (indent-according-to-mode)
  (previous-line)
  (indent-according-to-mode))

(define-key global-map (kbd "M-[") (lambda () (interactive) (4l/insert-block nil)))
(define-key global-map (kbd "M-{") (lambda () (interactive) (4l/insert-block t)))

;; move line up
(defun 4l/move-line-up ()
  (interactive)
  (transpose-lines 1)
  (previous-line 2))

(defun 4l/move-line-down ()
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))

(global-set-key (kbd "<M-up>") '4l/move-line-up)
(global-set-key (kbd "<M-down>") '4l/move-line-down)

(setq
 dabbrev-case-replace t
 dabbrev-case-fold-search t
 dabbrev-upcase-means-case-search t)

(abbrev-mode 1)

(when (and (require 'ls-lisp) (require 'dired))
  (setq
   ls-lisp-dirs-first t
   ls-lisp-use-insert-directory-program nil))

(require 'ido)
(setq
 ido-enable-flex-matching t
 ido-everywhere t)
(ido-mode t)

(define-key global-map (kbd "M-3")   'ibuffer)
(define-key global-map (kbd "C-c r") 'revert-buffer)

;; Souce: https://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs
(defun so/duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count))))

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))

(define-key global-map (kbd "C-M-d") #'so/duplicate-line)
(define-key global-map (kbd "C-S-s") #'isearch-forward-symbol-at-point)
(define-key global-map (kbd "C-j")   #'join-line)

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
(dolist (package '(projectile scala-mode rust-mode))
  (unless (package-installed-p package)
    (package-install package)))

;; NOTE:: Quick fix for projectile... not sure why it complains
(require 'subr-x)
(setq
 projectile-require-project-root  t
 projectile-mode-line            '(:eval (format " {%s}" (projectile-project-name)))
 projectile-git-submodule-command "git submodule --quiet foreach 'echo $path' | tr '\\r\\n' '\\0'"
 projectile-project-root-files-functions '(projectile-root-local projectile-root-top-down projectile-root-bottom-up projectile-root-top-down-recurring)
 projectile-enable-caching        t
 projectile-indexing-method      'alien)

(projectile-mode t)
(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map)
  (define-key global-map (kbd "M-1") #'projectile-find-file)

  ;; Would use a better compile command for bloop
  (projectile-register-project-type 'bloop '(".bloop")
                                    :compile "bloop compile --reporter scalac --no-color root"
                                    :test "bloop test --propagate --reporter scalac root"
                                    :src-dir "src/main/"
                                    :test-dir "src/test/"
                                    :test-suffix "Spec"))

(defconst ORG-BASE-FOLDER "~/Dropbox/Sandbox/Library/Org/")

(setq
 org-log-done                  'time ;; When completing a task, prompt for a closing note...
 org-src-fontify-natively       t
 org-descriptive-links          t
 org-startup-with-inline-images t
 
 org-list-description-max-indent 0
 
 org-enforce-todo-dependencies  t
 org-enforce-todo-checkbox-dependencies t
 
 org-catch-invisible-edits      'error
 
 org-clock-persist              t
 
 org-hide-leading-stars         nil
 org-line-spacing               5
 org-tags-column                0 ;; Have tags next to the title
 
 ;;org-babel-load-languages      '((sql . t) (shell . t) (plantuml . t))
 org-babel-C-compiler "clang"
 org-babel-C++-compiler "clang++"
 
 ;; Keywords
 org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "ACTIVE" "|" "DONE(d)" "SOMEDAY(s)" "CANCELLED(c)"))
 org-todo-keyword-faces '(("ACTIVE" . "yellow"))

 org-refile-use-outline-path 'file
 org-refile-targets '((org-agenda-files :maxlevel . 1))
 org-refile-target-verify-function #'(lambda () (member "project" (org-get-local-tags)))

 org-adapt-indentation nil

 org-archive-location "./archives/%s_archive::"
 
 org-capture-templates `(("n" "New"   entry (file ,(concat ORG-BASE-FOLDER "universe.org")) "* TODO %?")
                         ("t" "Today" entry (file ,(concat ORG-BASE-FOLDER "universe.org")) "* TODO %? \nSCHEDULED: %t"))
 
 org-agenda-files (list (concat ORG-BASE-FOLDER "universe.org"))
 
 org-agenda-custom-commands '(("c" . "My Custom Agendas")
                              ("cu"  "Unscheduled"
                               ((todo ""
                                      ((org-agenda-overriding-header "\nUnscheduled TODO")
                                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline)))))
                               nil ;; general settings for the whole set
                               nil))
 
 org-agenda-start-on-weekday 7 ;; Saturday
 org-agenda-include-diary nil
 org-agenda-span 'day
 org-agenda-skip-deadline-if-done t
 
 ;; Display agenda in full window
 org-agenda-window-setup 'current-window)

;; TODO: Add function to swap two windows 

(setq
 scala-indent:use-javadoc-style t
 scala-mode:debug-messages nil)

(defun 4l/scala/fix-scala-fonts ()
  (interactive)
  (mapc
   (lambda (kw)
     (let ((face-ref (intern (format "scala-font-lock:%s-face" kw))))
       (copy-face font-lock-keyword-face face-ref)))
   '("final" "private" "protected" "implicit" "abstract" "sealed" "lazy" "override" "inline")))

(add-hook #'scala-mode-hook #'4l/scala/fix-scala-fonts)

(setq
 rust-indent-offset  2
 rust-format-on-save nil)

;;(add-hook 'window-configuration-change-hook (lambda () (balance-windows)))

(defun 4l/pwsh ()
  "Run PowerShell in Emacs"
  (interactive)
  (let ((shell-file-name "C:\\Users\\Aleksandr\\scoop\\shims\\pwsh.exe"))
    (shell)))
