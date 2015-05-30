;;----------------------------------------------------------------------------
;; Delete the current file
;; By purcell
;; Taken from: https://github.com/purcell/emacs.d/blob/master/lisp/init-utils.el#L49
;;----------------------------------------------------------------------------
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?" (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;;----------------------------------------------------------------------------
;; Rename the current file
;; By purcell
;; Taken from: https://github.com/purcell/emacs.d/blob/master/lisp/init-utils.el#L62
;;----------------------------------------------------------------------------
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (when (file-exists-p filename)
         (rename-file filename new-name 1))
        (rename-buffer new-name)
        (set-visited-file-name new-name)))))

;;----------------------------------------------------------------------------
;; Duplicate current line
;; Taken from: http://stackoverflow.com/a/998472
;;----------------------------------------------------------------------------
(defun duplicate-line (arg)
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
;; Remove file assosiated with current buffer and close this buffer
;;----------------------------------------------------------------------------
(defun 4lex1v/delete-current-file ()
  (interactive)
  (let ((file-path (buffer-file-name)))
    (delete-file file-path)
    (kill-buffer current-buffer)))

;;----------------------------------------------------------------------------
;; Byte compile Emacs directory
;; Taken from: http://emacsredux.com/blog/2013/06/25/boost-performance-by-leveraging-byte-compilation/
;;----------------------------------------------------------------------------
(defun 4lex1v/byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

;;----------------------------------------------------------------------------
;; Alternative to add-hook
;; Taken from: https://github.com/jwiegley/dot-emacs/blob/master/init.el
;;----------------------------------------------------------------------------
(defsubst hook-into-modes (func &rest modes)
  (dolist (mode-hook modes) (add-hook mode-hook func)))

(defun create-eshell (_name)
  "Create an eshell buffer named NAME."
  (interactive "sName: ")
  (setq name _name)
  (eshell)
  (rename-buffer name))

(defun 4lex1v/multiWindow-p ()
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

(provide 'functions)
