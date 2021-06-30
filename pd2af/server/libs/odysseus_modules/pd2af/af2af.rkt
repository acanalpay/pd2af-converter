#lang racket

(require "../../odysseus/lib/_all.rkt")
(require "../../odysseus_modules/sbgn/common.rkt")
(require "../../odysseus_modules/sbgn/types.rkt")
(require "../../odysseus_modules/sbgn/context.rkt")
(require "../../odysseus_modules/sbgn/sexp.rkt")
(require "../../odysseus_modules/sbgn/geometry.rkt")
(require racket/syntax)

(provide (all-defined-out))

; element names consider equal if equal are their af-signatures
(define (get-af-signature el)
  (let* ((res
          (list ($ compartment el) ($ class el) ($ name el)))
        (res (if (ontology-uoi? ($ uoi el))
                  (append res (list ($ uoi el)))
                  res)))
    res))

(define (except-current-element el-id af-context)
  (filter-not (λ (e) (equal? ($ id e) el-id)) af-context))

(define-catch (remove-self-loops context)
  (for/fold
    ((res empty))
    ((e context))
    (let* ((sources ($ sources e))
          (targets ($ targets e))
          ; Self-looping: if element presents both in sources and targets - remove it from them
          (sources-and-targets (if (and sources targets)
                                    (intersect sources targets)
                                    #f))
          (new-sources (if sources-and-targets (minus sources targets) #f))
          (new-targets (if sources-and-targets (minus targets sources) #f)))
      (cond
        ; remove whole element, if both new sources and targets empty
        ((and (empty? new-sources) (empty? new-targets))
          res)
        ; remove rudimentary left-hander
        ((and new-sources new-targets (empty? new-targets))
          (pushr res (hash-union (hash 'sources new-sources 'targets targets) e)))
        ; remove rudimentary right-hander (is this situation possible?)
        ((and new-sources new-targets (empty? new-sources))
          (pushr res (hash-union (hash 'sources sources 'targets new-targets) e)))
        ; if no other elements to connect from sources or to targets - remove this arc:
        ((and new-sources new-targets (and (empty? new-sources) (empty? new-targets)))
          res)
        ; otherwise modify its sources and targets atrributes:
        ((and sources targets)
          (pushr res (hash-union (hash 'sources new-sources 'targets new-targets) e)))
        ; simply append the next non-arc element
        (else (pushr res e))))))

; remove state values from the names of AF element, if it is an only element with a 'root name'
; e.g. A-P B C B-PP D-P D-sumo -> A B C B-PP D-P D-sumo
(define-catch (strip-off-state-prefix-if-no-duplication context)
  (define-catch (name-root aname)
    (cond
      ((not aname) aname)
      (else
        (let* ((parts (string-split (->string aname) "-"))
              (root (first parts))
              (suffixes (rest parts))
              (suffixes (filter-not state-variable-value? suffixes)))
          (implode (append (list root) suffixes) "-")))))
  (define-catch (same-name-root? name1 name2)
      (equal? (name-root name1) (name-root name2)))
  (define-catch (get-same-namer el context)
    (let* ((current-id ($ id el))
          (current-name ($ name el))
          (same-namers
            (filter-not
              (λ (x)
                (or (equal? current-id ($ id x))
                    (not (same-name-root? current-name ($ name x)))))
            context)))
      same-namers))
  (for/fold
    ((res empty))
    ((el context))
    (begin
      ; (when ($ name el) (--- ($ name el) (has-variables? el) (get-same-namer el context)))
      (cond
        ((not (has-variables? el)) (pushr res el))
        ((and ($ name el) (empty? (get-same-namer el context)))
          (pushr res (hash-union (hash 'name (name-root ($ name el))) el)))
        (else (pushr res el))))))
