#lang racket/base

(require deta
         racket/bool
         racket/contract
         racket/format
         racket/match
         racket/struct
         threading
         "create.rkt")

(provide (all-defined-out))

(define/contract id?
  (-> any/c boolean?)
  exact-nonnegative-integer?)

(define/contract (schema-symbol? sym)
  (-> symbol? boolean?)
  (and (member sym '(familia<sql> genus<sql> species<sql> plant<sql>)) #t))

(define/contract (insert-familia! latin)
  (-> string? (or/c #f entity?))
  (insert-one! vc (make-familia<sql> #:latin latin)))

(define/contract (insert-genus! latin [id 0])
  (->* (string?) (id?) (or/c #f entity?))
  (insert-one! vc (make-genus<sql> #:latin latin #:id<familia> id)))

(define/contract (insert-species! latin [id 0])
  (->* (string?) (id?) (or/c #f entity?))
  (insert-one! vc (make-species<sql> #:latin latin #:id<genus> id)))

(define/contract (insert-plant! french [id 0])
  (->* (string?) (id?) (or/c #f entity?))
  (insert-one! vc (make-plant<sql> #:french french #:id<species> id)))

(define/contract (query-list-all schema-sym)
  (-> schema-symbol? (listof entity?))
  (query-list (~> (from ,schema-sym #:as sch))))

(define/contract (query-from-name schema-sym str)
  (-> schema-symbol? string? (or/c #f entity?))
  (cond
    [(symbol=? schema-sym 'plant<sql>)
     (lookup vc (~> (from ,schema-sym #:as sch) (where (= sch.french ,str))))]
    [else (lookup vc (~> (from ,schema-sym #:as sch) (where (= sch.latin ,str))))]))

(define/contract (query-like-name schema-sym str)
  (-> schema-symbol? string? (or/c #f (listof entity?)))
  (cond
    [(symbol=? schema-sym 'plant<sql?)
     (query-list (~> (from ,schema-sym #:as sch) (where (like sch.french ,str))))]
    [else (query-list (~> (from ,schema-sym #:as sch) (where (like sch.latin ,str))))]))

(define/contract (entity-ref col-key entity)
  (-> symbol? entity? any)
  (for/or ([(key value) (in-hash (entity->hash entity))])
    ;; (println (~a "K: " key " " "V: " value))
    (match key
      [col-key value]
      [_ #f])))

;; (define/contract (entity-update! schema-sym id . args)
;;   ()
(define/contract (record-plant familia genus species french)
  (-> string? string? string? string? (values id? id? id? (or/c entity? #f)))
  (define familia-id
    (entity-ref 'id
                (or (query-from-name 'familia<sql> (string-titlecase familia))
                    (insert-familia! familia))))

  (define genus-id
    (entity-ref 'id
                (or (query-from-name 'genus<sql> (string-titlecase genus))
                    (insert-genus! genus familia-id))))

  (define species-id
    (entity-ref 'id
                (or (query-from-name 'species<sql> (string-downcase species))
                    (insert-species! species genus-id))))

  (values familia-id
          genus-id
          species-id
          (or (query-from-name 'plant<sql> (string-titlecase french))
              (insert-plant! french species-id))))
