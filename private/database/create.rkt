#lang racket/base

(require db
         deta
         racket/contract
         racket/string
         "../../init.rkt")

(provide (all-defined-out))

(define vc (virtual-connection (λ () (sqlite3-connect #:database DB-PATH #:mode 'create))))

(define/contract (query-list q)
  (-> query? (or/c (listof entity?) #f))
  (for/list ([e (in-entities vc q)])
    e))

(define-schema familia<sql>
               #:table "families"
               ([id id/f #:primary-key #:auto-increment]
                [latin string/f #:contract non-empty-string? #:wrapper string-titlecase]))

(define-schema genus<sql>
               #:table "genera"
               ([id id/f #:primary-key #:auto-increment]
                [latin string/f #:contract non-empty-string? #:wrapper string-titlecase]
                [id<familia> id/f]))

(define-schema species<sql>
               #:table "species"
               ([id id/f #:primary-key #:auto-increment]
                [latin string/f #:contract non-empty-string? #:wrapper string-downcase]
                [id<genus> id/f]))

(define-schema plant<sql>
               #:table "plants"
               ([id id/f #:primary-key #:auto-increment]
                [french string/f #:contract non-empty-string? #:wrapper string-titlecase]
                [id<species> id/f]))

(create-all! vc)
;; (void (insert! vc (make-familia<sql> #:latin "Unknown"))
;;       (insert! vc (make-genus<sql> #:latin "Unknown" #:id<familia> 0))
;;       (insert! vc (make-species<sql> #:latin "Unknown" #:id<genus> 0)))
