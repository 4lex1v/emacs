(use-package pdf-tools
  :load-path "modules/reader/pdf-tools/server"
  :mode ("\\.pdf\\'" . pdf-view-mode)
  
  :init
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-resize-factor 1.10)
  
  :config
  (pdf-tools-install))

