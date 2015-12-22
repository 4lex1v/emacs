(add-to-list 'load-path (concat user-emacs-directory "packages/elisp/macrostep"))

(defun wrap-with-parens (&optional arg)
  (interactive "P")
  (sp-wrap-with-pair "(")
  (sp-forward-slurp-sexp))

(provide 'elisp)
