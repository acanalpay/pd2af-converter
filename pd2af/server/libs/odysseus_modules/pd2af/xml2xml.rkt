#lang racket

(require "../../odysseus/lib/_all.rkt")

(provide (all-defined-out))

(define-catch (clean-sbgn-ml xml)
  (let* ((xml (string-replace xml #px"<extension>.*?</extension>" "")))
    xml))
