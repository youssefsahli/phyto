;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((racket-mode .
              ((apheleia-formatter . raco-fmt)
               (apheleia-formatters . ((raco-fmt "raco" "fmt" "--width" "80")))
               (eval . (racket-xp-mode)))))
