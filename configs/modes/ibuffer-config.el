;; IBUFFER
(setq ibuffer-default-sorting-mode 'major-mode)

;; Hide empty groups
(setq ibuffer-show-empty-filter-groups nil)

;; Group by projectile roots
(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-projectile-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))
