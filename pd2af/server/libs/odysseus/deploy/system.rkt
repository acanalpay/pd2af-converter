#lang racket

(require racket/file)
(require "../lib/_all.rkt")
(require compatibility/defmacro)

(provide (all-defined-out))

(define (where-am-i)
    'lcsb)

(define (server?)
  (case (where-am-i)
    ((inwin-win7 unknown-win) #f)
    ((digitalocean) #t)
    (else #f)))

(define-macro (-s . exprs)
  `(when (server?) ,@exprs))
