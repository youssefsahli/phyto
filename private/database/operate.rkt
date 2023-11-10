#lang racket/base

(require deta
         deta/reflect
         racket/bool
         racket/contract
         racket/match
         threading
         "create.rkt")

(define/contract id?
  (-> any/c boolean?)
  exact-nonnegative-integer?)

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

(define/contract (query-from-name schema str)
  (-> symbol? string? (or/c #f entity?))
  (cond
    [(symbol=? schema 'plant<sql>)
     (lookup vc (~> (from ,schema #:as sch) (where (= sch.french ,str))))]
    [else (lookup vc (~> (from ,schema #:as sch) (where (= sch.latin ,str))))]))

(define/contract (entity-id entity)
  (-> entity? id?)
  (for/or ([(key value) (in-hash (entity->hash entity))])
    (match key
      ['id value]
      [_ #f])))

(define/contract (record-plant familia genus species french)
  (-> string? string? string? string? (or/c entity? #f))
  (define familia-id
    (entity-id (or (query-from-name 'familia<sql> (string-titlecase familia))
                   (insert-familia! familia))))

  (define genus-id
    (entity-id (or (query-from-name 'genus<sql> (string-titlecase genus))
                   (insert-genus! genus familia-id))))

  (define species-id
    (entity-id (or (query-from-name 'species<sql> (string-downcase species))
                   (insert-species! species genus-id))))

  (or (query-from-name 'plant<sql> (string-titlecase french))
      (insert-plant! french species-id)))
