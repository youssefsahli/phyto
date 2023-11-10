#lang racket/base

(require db
         racket/contract
         "../../init.rkt")

(define/contract (virtual-connect)
  (-> connection-pool?)
  (virtual-connection (λ ()
                        (sqlite3-connect #:database DB-PATH #:mode 'create))))
