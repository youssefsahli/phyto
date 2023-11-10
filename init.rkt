#lang racket/base

(require racket/runtime-path
         racket/contract)

(provide/contract [DB-PATH path-string?])

(define-runtime-path DB-PATH "phyto-db.sqlite3")
