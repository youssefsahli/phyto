#lang racket/base

(require deta
         racket/class
         racket/contract
         racket/match
         "../database/operate.rkt")

(provide phyto-application<%>
         phyto-application%)

(define/contract phyto-application<%>
  interface?
  (interface ()
    [get-schema-list (-> schema-symbol? (listof string?))]
    [insert-entity! (-> schema-symbol? entity? (or/c entity? #f))]))

(define phyto-application%
  (class* object%
          (phyto-application<%>)
          (super-new)
          (define/public (get-schema-list schema-sym)
            (map extract-name (query-list schema-sym)))
          (define/public (make-insert-entity! schema-sym . args)
            (let ([func (match schema-sym
                          ['familia<sql> insert-familia!]
                          ['genus<sql> insert-genus!]
                          ['species<sql> insert-species!]
                          ['plant<sql> insert-plant!])])
              (apply func args)))))
