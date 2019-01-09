;; -*- lexical-binding: true -*-

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

(defun use-package-handler/:hooks (package-name-symbol hooks-kw args rest state)
  (let* ((modes-hook (normalize-hook-name package-name-symbol args))
         (hooks-tree
          (build-hooks-tree modes-hook (if (keywordp (car args))
                                           (cdr args)
                                         args)))
         (updated-config (inject-tree hooks-tree rest)))
    (use-package-process-keywords package-name-symbol updated-config state)))

;; #NOTE :: Should return a string
(defun normalize-hook-name (package-name-symbol args-list)
  "`normalize-hook-name' accepts two arguments: a symbolic pacakage name and provided list of arguments. First it check the first argument of an ARGS-LIST,
if it's a keyword like \":c-mode-common\", it's used as a hook name, otherwise
we normalize a package name and return it as a string"
  (if (keywordp (car args-list))
      (substring (symbol-name (car args-list)) 1)
    (let ((qualified-name (symbol-name package-name-symbol)))
      (if (s-ends-with-p "-hook" qualified-name)
          qualified-name
        (concat qualified-name "-hook")))))

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


(setq use-package-keywords
      (upe--insert-after-kw :demand :hooks))

(provide 'upe-hooks)
