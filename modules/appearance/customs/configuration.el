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
;;(defcustom default-font-name "Ayuthaya"
  "Font name used by default"
  :group 'string 
  :options '("PragmataPro"
             "Menlo"
             "Ayuthaya"))

(defcustom default-font-size
  (if (or IS_MAC IS_LINUX)
      (pcase default-font-name
        ("PragmataPro" 18)
        ("Monaco"      14)
        ("Ayuthaya"    13)
        (t             18))
       13)
  "Default Font size used across all frames")

(defun change-font-size (value)
  (interactive
   (list
    (read-number (format "Font current font size [%d] to: " font-size))))
  (setq font-size value)
  (4lex1v:gui:font font-name :size value))

;; For some reason i need this workaround otherwise Emacs is tooooo
;; slow when i'm using Windows. Pretty sure there's a better solution
(if IS_WINDOWS
    (progn
      (set-face-attribute 'default nil
                          :font "PragmataPro"
                          :height (* default-font-size 10)))

  ;; Will be configured for Mac or Linux
  (let ((frame-font (format "%s-%d"
                            default-font-name
                            default-font-size)))
    (set-frame-font frame-font)
    (add-to-list 'default-frame-alist (cons 'font frame-font))
    (set-face-attribute 'default nil :height (* default-font-size 10))
    (4lex1v:gui:frame :transparency '(100 . 100)
                      :cursor       '(box . bar))))

;; #NOTE(4lex1v, 08/24/17) :: Split the window from the very begging
(split-window-right)

;; #NOTE(4lex1v, 08/24/17) :: Initial frame configuration
;; #TODO(4lex1v, 08/24/17) :: Wonder if i should have this setup for any Emacs of
;;                            or for specific builds only ???
(add-to-list 'initial-frame-alist (cons 'left 50))

(add-to-list 'initial-frame-alist
             (cons 'height
                   (let ((one (/ (display-pixel-height)
                                 (frame-char-height))))
                     (- one (/ one 2.5)))))

(add-to-list 'initial-frame-alist
             (cons 'width
                   (/ (/ (ceiling (* (- (display-pixel-width)
                                      (apply '+ (cl-remove-if (lambda (i) (not i))
                                                              (window-fringes))))
                                   0.99))
                       (frame-char-width)) 3)))

;; #NOTE(4lex1v, 08/24/17) :: Ref: `font-lock-keywords`
;; #TODO(4lex1v) :: Need to add some support in combination with projectile to see all entries in the project
;; #TODO(4lex1v) :: Need to update the visual repr of these things
;; #NOTE(4lex1v) :: Wonder if i can simple use `prog-mode` to enable the highliting everywhere?
(mapc
 (lambda (mode)
   (font-lock-add-keywords ;;`font-lock-keywords`
    mode
    '(("#\\<\\(TODO\\)\\>" 1 '(error :underline t) t)
      ("#\\<\\(NOTE\\)\\>" 1 '(warning :underline t) t))))
   '(emacs-lisp-mode scala-mode c-mode c++-mode))

;; #NOTE(4lex1v, 08/24/17) :: Default to an empty string that should be introduced manually
(setq comment-note-comment-prefix "")

;; #TODO(4lex1v, 08/24/17) :: 


