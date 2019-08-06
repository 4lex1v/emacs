
;; http://www.wilfred.me.uk/blog/2014/09/27/the-definitive-guide-to-syntax-highlighting/

(defconst rye-lang-mode-syntax-table
  ;; (let ((table (make-syntax-table)))
  (let ((table (copy-syntax-table c-mode-syntax-table)))
    ;; ' is a string delimiter
    (modify-syntax-entry ?' "\"" table)
    ;; " is a string delimiter too
    (modify-syntax-entry ?\" "\"" table)

    ;; # Comment starter
    (modify-syntax-entry ?# "<" table)

    ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" table)
    table))

(setq rye-lang-font-lock-keywords
      (let* ((x-keywords '("return" "use" "if" "else" "elif" "for" "while" "match" "case" "struct" "enum" "as"))
             (x-keywords-re (regexp-opt x-keywords 'words)))
        `((,x-keywords-re . font-lock-keyword-face))))

;;;###autoload
(define-derived-mode rye-lang-mode prog-mode "Rye"
  :syntax-table rye-lang-mode-syntax-table
  (setq font-lock-defaults '((rye-lang-font-lock-keywords)))
  (setq comment-start "#")
  (font-lock-fontify-buffer))

(provide 'rye-lang-mode)
