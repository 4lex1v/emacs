;; Scala-mode configuration

;; Load ensime mode for scala only if there is an ensime
;; project file .ensime defined in the root directory
(defun 4lex1v/smart-ensime-loading ()
  (let* ((root-dir            (projectile-project-root))
         (ensime-project-file (concat root-dir ".ensime"))
         (ensime-project?     (file-exists-p ensime-project-file)))
    (if ensime-project? (ensime-scala-mode-hook))))

;; Indent new line between braces with smartparens mode
(defun 4lex1v/indent-in-braces (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(add-hook 'scala-mode-hook 
          '(lambda ()
             (yas-minor-mode)
             (hs-minor-mode)
             (4lex1v/smart-ensime-loading)
             (smartparens-mode)))


;; Smartparen configuration for scala-mode
(sp-local-pair 'scala-mode "{" nil
               :post-handlers '((4lex1v/indent-in-braces "RET")))
(setq scala-indent:use-javadoc-style t)
