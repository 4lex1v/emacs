(setq-default
 mode-line-default-help-echo nil ; turn-off tooltips on cursor hover-over
 tab-width           2 ;; Though i'm not using tabs
 indent-tabs-mode    nil
 cursor-type        'box
 cursor-in-non-selected-windows 'bar
 frame-title-format " %@%b% -"
 linum-format       "%3d "  ;; Try dynamic?
 load-prefer-newer  t
 left-fringe-width  20
 word-wrap t)

(global-hl-line-mode t)
(show-paren-mode     t)
(delete-selection-mode t)
(toggle-truncate-lines nil)

(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(when (boundp 'window-divider-mode)
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1)
  (window-divider-mode +1))

(add-hook 'window-configuration-change-hook
          (lambda ()
            (set-window-margins (get-buffer-window (current-buffer) nil) 5 3)))

(defcustom default-font-name "PragmataPro"
;(defcustom default-font-name "Menlo"
  "Font name used by default"
  :group 'string 
  :options '("PragmataPro"
             "Menlo"
             "Ayuthaya"))

(defcustom default-font-size
  (if (or IS_MAC IS_LINUX) 18 13)
  "Default Font size used across all frames")

(defun change-font-size (value)
  (interactive
   (list
    (read-number (format "Font current font size [%d] to: " font-size))))
  (setq font-size value)
  (4lex1v:gui:font font-name :size value))

;; For some reason i need this workaround otherwise Emacs is tooooo
;; slow when i'm using Windows. Pretty sure there's a better solution
(if (or IS_MAC IS_LINUX)
    (let ((frame-font (format "%s-%d"
                              default-font-name
                              default-font-size)))
      (set-frame-font frame-font)
      (add-to-list 'default-frame-alist (cons 'font frame-font)))
  (set-face-attribute 'default nil :height (* default-font-size 10)))

(4lex1v:gui:frame :transparency '(100 . 100)
                  :cursor       '(box . bar))

