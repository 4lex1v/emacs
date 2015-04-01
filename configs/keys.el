;; GENERAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "RET")         'newline-and-indent)
(global-set-key (kbd "M-j")         'join-line)
(global-set-key (kbd "C-c m")       'execute-extended-command)
(global-set-key (kbd "C-x C-b")     'ibuffer)
(global-set-key (kbd "C-c l")       'view-mode)
(global-set-key (kbd "C-a")         'back-to-indentation)
(global-set-key (kbd "M-m")         'beginning-of-line)
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)
(global-set-key (kbd "C-S-d")       'duplicate-line)
(global-set-key (kbd "C-x f")       'other-frame)

;; SCALA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For some reason std indent-new-comment-line doesn't work with scala comments
(define-key scala-mode-map (kbd "RET") 'scala-functions:newline-or-comment)
(define-key scala-mode-map (kbd "M-j") 'scala-indent:join-line)

;; ENSIME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key ensime-mode-map (kbd "C-c b") 'sbt-ext:open-build-file)
(define-key scala-mode-map  (kbd "C-c e") 'ensime-print-errors-at-point)
(define-key scala-mode-map  (kbd "C-c t") 'ensime-print-type-at-point)
(define-key scala-mode-map  (kbd "C-c i") 'ensime-import-type-at-point)

;; SHELL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key shell-mode-map (kbd "C-c k") 'shell-clear)

;; PROJECTILE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (global-set-key (kbd "M-1") 'projectile-find-file)

;; HELM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(global-set-key (kbd "C-c h o")    'helm-occur)
(global-set-key (kbd "M-y")        'helm-show-kill-ring)
(global-set-key (kbd "C-x b")      'helm-mini)
(global-set-key (kbd "C-x C-f")    'helm-find-files)
(global-set-key (kbd "M-1")        'helm-projectile)
(global-set-key (kbd "M-x")        'helm-M-x)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i")   'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")   'helm-select-action) ; list actions using C-z
(define-key helm-map (kbd "C-o")   'helm-next-source)
(define-key helm-map (kbd "M-o")   'helm-previous-source)

(global-set-key (kbd "C-=") 'er/expand-region)

;; MAGIT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c s") 'magit-status)

;; SMEX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; ACE-JUMP-MODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;; NEOTREE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [f8] '4lex1v/neotree-projectile-toggle)

;; HIDESHOW ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c [") 'hs-hide-block)
(global-set-key (kbd "C-c ]") 'hs-show-block)

(provide 'keys)
