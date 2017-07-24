
;; Utils
(defun upe--insert-after (list elem insert-elem)
  "Insert `insert-elem' after `elem' in the provided `list'"
  (cond ((eq nil list) (list insert-elem))
        ((eq elem (car list)) (cons elem (cons insert-elem (cdr list))))
        (t (cons (car list) (upe--insert-after (cdr list) elem insert-elem)))))

(defun upe--insert-after-kw (mark-kw new-kw)
  (upe--insert-after use-package-keywords mark-kw new-kw))

;; This dude receives a list of hooks provided under the keyword
;; (defun use-package-normalize/:hooks (package-name-s hooks-kw args)
;;   `,args)
(defalias 'use-package-normalize/:hooks 'use-package-normalize-symlist)

(defun normalize-hook-name (package-name)
  (if (s-ends-with-p "-mode" package-name)
      (concat package-name "-hook")
    (concat package-name "-mode-hook")))

(defun build-hooks-tree (hook-name hooks)
  (mapcar (lambda (hook) `(add-hook #',(intern hook-name) ',hook)) hooks))

;;;; PList manipulation funcs
(defun update-plist-entry (plist kw upd-func)
  (declare (indent 2))
  (let* ((old-entry (plist-get plist kw))
         (new-value  (funcall upd-func old-entry)))
    (plist-put plist kw new-value)))
;;;; End

(defun inject-tree (tree rest)
  (update-plist-entry rest :config
    (lambda (old-config)
      (use-package-concat tree old-config))))

(defun use-package-handler/:hooks (package-name-s hooks-kw args rest state)
  (let* ((modes-hook (normalize-hook-name (symbol-name package-name-s)))
        (hooks-tree (build-hooks-tree modes-hook args))
        (updated-config (inject-tree hooks-tree rest)))
    (use-package-process-keywords package-name-s updated-config state)))

(setq use-package-keywords
      (upe--insert-after-kw :demand :hooks))

(provide 'upe-hooks)
