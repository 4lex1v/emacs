;; Investigate various elisp compilation options...

(defun 4lex1v/configure-font (name size)
  (let ((frame-font (format "%s-%d" name size)))
    ;; Set font for current frame 
    (set-frame-font frame-font) 

    ;; Set default font for new frames
    (add-to-list 'default-frame-alist   
                 (cons 'font (format "%s-%d" name size)))))

(defun 4lex1v/configure-frame-size (size-param)
  ;; Set frame size for current frame
  (set-frame-parameter nil 'fullscreen size-param)
  
  ;; Set Default frame size param
  (add-to-list 'default-frame-alist
               (cons 'fullscreen size-param)))

(defun 4lex1v/transparent-ui (v1 v2)
  (set-frame-parameter (selected-frame) 'alpha (cons v1 v2))
  (add-to-list 'default-frame-alist (cons 'alpha (cons v1 v2))))

;;----------------------------------------------------------------------------
;; Duplicate current line
;; Taken from: http://stackoverflow.com/a/998472
;;----------------------------------------------------------------------------
(defun 4lex1v/duplicate-line (arg)
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
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))

;;----------------------------------------------------------------------------
;; Alternative to add-hook
;; Taken from: https://github.com/jwiegley/dot-emacs/blob/master/init.el
;;----------------------------------------------------------------------------
(defsubst 4lex1v/hook-into-modes (func &rest modes)
  (dolist (mode-hook modes) (add-hook mode-hook func)))

(defun 4lex1v/multi-window-p ()
  (> (count-windows) 1))

(defun 4lex1v/closeBuffer (&optional arg)
  "Close currently opened file (i.e assosiated buffer) and
if the arg is not nil and it's not the single window, close it as well"
  (interactive "P")
  (kill-buffer (current-buffer))
  (if (and (not (equal arg 'nil))
           (4lex1v/multiWindow-p))
      (delete-window)))

(defun 4lex1v/closeOtherBuffer ()
  (interactive)
  (if (4lex1v/multiWindow-p)
      (progn
        (other-window 1)
        (4lex1v/closeBuffer t))))

(defun 4lex1v/forProjectileProject (func)
  (if (projectile-project-p)
      (func)
    (error "Not a projectile project!")))


(provide 'core-defuns)
