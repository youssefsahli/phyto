#lang info

(define name "PhytoCAR")
(define collection name)
(define version "0.1")
(define deps (list (list "base") (list "mischief") (list "gui-easy")))
(define license 'GPL-3.0-or-later)

(define gracket-launcher-names (list "phyto-car"))
(define gracket-launcher-libraries (list "src/phyto.rkt"))
(define pkg-authors (list "Y. Sahli"))
