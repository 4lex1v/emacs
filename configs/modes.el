;; Config for plugins and modes

(defun 4lex1v/enable-plugins (plugins)
	;; load provided packages 
	(mapc (lambda (plugin) (require plugin))
				plugins)
	
	;; load custom configs
	(mapc (lambda (config-file) (load config-file))
					(directory-files "~/.emacs.d/configs/modes/" t ".el")))
					
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(4lex1v/enable-plugins
 '(
	 org-install 
	 
	 ;; Scala lang
	 scala-mode2
	 sbt-mode
	 ensime

	 ;; Haskell lang
	 haskell-mode

	 ;; Helm and Projectile 
	 projectile
	 helm-config
	 helm-projectile
	 helm-descbinds

	 ;; YASnippet
	 yasnippet

	 smartparens-config
	 
	 ;; Others
	 ace-jump-mode
	 auto-complete-config))

(provide 'modes)
