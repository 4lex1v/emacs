(setq-local
 --magit-submodule-resized-table
 (quote
  (("Path" 50 magit-modulelist-column-path nil)
   ("Version" 35 magit-repolist-column-version nil)
   ("Branch" 20 magit-repolist-column-branch nil)
   ("L<U" 3 magit-repolist-column-unpulled-from-upstream
    ((:right-align t)))
   ("L>U" 3 magit-repolist-column-unpushed-to-upstream
    ((:right-align t)))
   ("L<P" 3 magit-repolist-column-unpulled-from-pushremote
    ((:right-align t)))
   ("L>P" 3 magit-repolist-column-unpushed-to-pushremote
               ((:right-align t))))))
