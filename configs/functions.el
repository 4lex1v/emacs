;; Configuration file for custom defined functions
(defconst custom-functions (directory-files "~/.emacs.d/configs/functions/" t ".el"))

(mapc 'load custom-functions) 

(provide 'functions)					
