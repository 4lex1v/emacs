;; Folder with main configuration files for Emacs
(defconst configs-dir   (expand-file-name "configs" user-emacs-directory))

;; Contains configuration files for all used packages
(defconst modes-dir     (expand-file-name "modes"   user-emacs-directory))
(defconst mode-files    (directory-files modes-dir t ".el"))

;; Add both folder to the load path
(add-to-list 'load-path configs-dir)
(add-to-list 'load-path modes-dir)

(require 'packages)         ;; init package system
(require 'benchmark-init)   ;; benchmark emacs loading time
(benchmark-init/activate)   ;; start benchmarking

;; Macro that's used to do the configuration
(require 'use-package)

;; Emacs configuration
(use-package ui)
(use-package general)

(bind-key "RET"         'newline-and-indent)
(bind-key "M-j"         'join-line)
(bind-key "C-c m"       'execute-extended-command)
(bind-key "C-x C-b"     'ibuffer)
(bind-key "C-c l"       'view-mode)
(bind-key "C-a"         'back-to-indentation)
(bind-key "M-m"         'beginning-of-line)
(bind-key "S-C-<left>"  'shrink-window-horizontally)
(bind-key "S-C-<right>" 'enlarge-window-horizontally)
(bind-key "S-C-<down>"  'shrink-window)
(bind-key "S-C-<up>"    'enlarge-window)
(bind-key "C-S-d"       'duplicate-line)
(bind-key "C-x f"       'other-frame)
(bind-key "C-c r"       'revert-buffer)

(mapc 'load mode-files)

(benchmark-init/deactivate) ;; stop benchmarking
