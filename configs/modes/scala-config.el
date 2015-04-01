;; Scala-mode configuration

(defun 4lex1v/ensime-project? ()
  (interactive)
  (let* ((root-dir (projectile-project-root))
         (ensime-project-file (concat root-dir ".ensime")))
    (file-exists-p ensime-project-file)))

;; Load ensime mode for scala only if there is an ensime
;; project file .ensime defined in the root directory
(defun 4lex1v/smart-ensime-loading ()
  (if (4lex1v/ensime-project?)
      (ensime-scala-mode-hook)))

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
(sp-local-pair 'scala-mode "/**" "*/")
(setq scala-indent:use-javadoc-style t)
