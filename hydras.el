(with-eval-after-load 'smartparens 
  (defhydra hydra-smartparens (:color pink :hint nil)
    
    "
^Slurp^         ^Barfs^
--------------------
_l_: f-slurp    _L_: f-barf
_h_: b-slurp    _H_: b-barf
--------------------
"
    
    ("l" sp-forward-slurp-sexp)
    ("h" sp-backward-slurp-sexp)
    ("L" sp-forward-barf-sexp)
    ("H" sp-backward-barf-sexp)
    ("q" nil "cancel")))


