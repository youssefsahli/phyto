#lang racket/base

(provide media)

(define MEDIA-PATH "../media/") ;Phase 0 binding

(define-syntax-rule (media name) (build-path MEDIA-PATH name))
