(defun 4lex1v:ui/configure-font (name size)
  (let ((frame-font (format "%s-%d" name size)))
    ;; Set font for current frame 
    (set-frame-font frame-font) 

    ;; Set default font for new frames
    (add-to-list 'default-frame-alist   
                 (cons 'font (format "%s-%d" name size)))))

(defun 4lex1v:ui/configure-frame-size (size-param)
  ;; Set frame size for current frame
  (set-frame-parameter nil 'fullscreen size-param)
  
  ;; Set Default frame size param
  (add-to-list 'default-frame-alist
               (cons 'fullscreen size-param)))

(defun 4lex1v:ui/transparent-ui (v1 v2)
  (set-frame-parameter (selected-frame) 'alpha (cons v1 v2))
  (add-to-list 'default-frame-alist (cons 'alpha (cons v1 v2))))

(provide 'ui)

