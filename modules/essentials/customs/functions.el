;; BACKUP from Bootstrap
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

(defun 4lex1v/close-buffer (&optional arg)
  "Close active buffer if a single window environment or close buffer with corresponding window
in multi-window environment. In order to leave the window opened provided an optional arg `leave-window'"
  (interactive "P")
  (kill-buffer (current-buffer))
  (if (and (not (equal arg 'nil))
           (4lex1v/multi-window-p))
      (delete-window)))

(defun 4lex1v:w/close-other-window ()
  "In a multi window environment close other (i.e not active) window. If there're more
then two windows around, provide an index number which window to close"
  (interactive)
  (if (4lex1v/multi-window-p)
      (progn
        (other-window 1)
        (kill-buffer (current-buffer))
        (delete-window))))     

(defmacro func (name &rest body)
  "Shortcut for basic interactive no-arg functions"
  `(defun ,name ()
     (interactive)
     ,@body))

(defmacro -func (body)
  `(lambda ()
     (interactive)
     ,@body))

(defsubst if-bound-f (f &optional args)
  "Helper function to guard against unbound functions"
  (if (fboundp ',f) (funcall ',f args)))

(defun toggle-comment-on-line ()
  (interactive)
  (comment-or-uncomment-region
   (line-beginning-position)
   (line-end-position)))

(defun open-init-file ()
  (interactive)
  (find-file user-init-file))

(defun string/starts-with (string prefix)
  "Return t if STRING starts with prefix."
  (and (string-match (rx-to-string `(: bos ,prefix) t)
                     string)
       t))

(defun contextual-backspace ()
  "Hungry whitespace or delete word depending on context."
  (interactive)
  (if (looking-back "[[:space:]\n]\\{2,\\}" (- (point) 2))
      (while (looking-back "[[:space:]\n]" (- (point) 1))
        (delete-char -1))
    (cond
     ((and (boundp 'smartparens-strict-mode)
           smartparens-strict-mode)
      (sp-backward-kill-word 1))
     ((and (boundp 'subword-mode) 
           subword-mode)
      (subword-backward-kill 1))
     (t
      (backward-kill-word 1)))))

(defun mac-os-p ()
  (interactive)
  (string-equal system-type "darwin"))
