;; Scala-mode configuration

(add-hook 'scala-mode-hook 
  '(lambda ()
		 (yas-minor-mode)
		 (hs-minor-mode)
		 (ensime-scala-mode-hook)
		 (smartparens-mode)))

;; Scala-mode smartparens configuration

;; Indent new line between braces with smartparens mode
(defun 4lex1v/indent-in-braces (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

;; Smartparen configuration for scala-mode
(sp-local-pair 'scala-mode "{" nil :post-handlers '((4lex1v/indent-in-braces "RET")))

(setq scala-indent:use-javadoc-style t)
