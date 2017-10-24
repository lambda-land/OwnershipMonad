(TeX-add-style-hook
 "ownership_monad"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("beavtex" "onehalf" "11pt")))
   (TeX-run-style-hooks
    "latex2e"
    "beavtex"
    "beavtex11"
    "algorithm"
    "algorithmic")
   (LaTeX-add-bibliographies))
 :latex)

