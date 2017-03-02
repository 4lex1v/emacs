;; Window management

(which-key-declare-prefixes "<SPC> w" "windows")
(evil-leader/set-key "wo"  'other-window)

(which-key-declare-prefixes "<SPC> w s" "spliting")
(evil-leader/set-key "wsb" 'split-window-below)
(evil-leader/set-key "wsh" 'split-window-horizontally)

(which-key-declare-prefixes "<SPC> w d" "deliting")
(evil-leader/set-key "wdd" 'delete-window)
(evil-leader/set-key "wdo" 'delete-other-windows)
 
;; File management
(which-key-declare-prefixes "<SPC> f" "files")
(func init.el (find-file (concat user-emacs-directory "/" "init.el")))
(evil-leader/set-key "fi" 'init.el)
(evil-leader/set-key "ff" 'helm-find-files)

