#lang racket

(require racket/class
         racket/gui/base
         deta
         "../database/operate.rkt"
         "../phyto-app/main.rkt")

(define/contract main-app (new phyto-application%))

(define/contract entity-panel<%>
  interface?
  (interface (subwindow<%> area-container-window<%>)
    [edit (-> entity? any)]
    [save (-> entity? (or/c entity? #f))]))

(define/contract entity-panel%
  (class/c (init [parent (is-a?/c frame%)]
                 [schema schema-symbol?]))

  (class* vertical-panel%
          (entity-panel<%>)
          (init parent)
          (super-new [parent parent])
          (define/public (edit e) '())
          (define/public (load e) '())
          (define/public (save) '())))

(define main-app-frame%
  (class* frame%
          (top-level-window<%>)
          (super-new [label "PhytoDB"] [width 640] [height 800])
          (define/public (run) (send this show #t))))

(define (run-app)
  (send (new main-app-frame%) run))
