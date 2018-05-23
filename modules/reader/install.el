
(use-package elfeed
  :general
  ("se" 'elfeed)

  (:keymaps 'elfeed-search-mode-map
   :states  'normal
   :prefix   nil

   "SPC" 'elfeed-control-panel/body)
  
  :init 
  (setq elfeed-search-filter "@1-month-ago +unread" ;; Default filter
        elfeed-feeds '(("https://bartoszmilewski.com/feed" FP)

        elfeed-db-directory "~/Dropbox/Приложения/elfeeddb"
        
        elfeed-mac-connections 10
        url-queue-timeout 30)

  (defhydra elfeed-control-panel ()
    "elfeed"
    ("u" elfeed-update "update" :color blue)
    ("q" nil "quit" :color blue))
  
  :config
  (with-eval-after-load 'evil-collection
    (add-to-list 'evil-collection-mode-list 'elfeed)
    (evil-collection-init)))
