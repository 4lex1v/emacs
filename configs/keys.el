;; GENERAL
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-j") 'join-line)
(global-set-key (kbd "C-c m") 'execute-extended-command) 

;; PROJECTILE
(global-set-key (kbd "M-1") 'projectile-find-file)

;; SMEX
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; ACE-JUMP-MODE
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;; NEOTREE
(global-set-key [f8] 'neotree-toggle)
