#lang racket

(require racket/syntax)
(require compatibility/defmacro)
(require (for-syntax racket/list racket/match "../../odysseus/lib/io.rkt"))
(require sxml)
(require "../../odysseus/lib/_all.rkt")
(require "../../odysseus_modules/sbgn/common.rkt")
(require "../../odysseus_modules/sbgn/types.rkt")
(require "../../odysseus_modules/sbgn/context.rkt")
(require "../../odysseus_modules/sbgn/sexp.rkt")
(require "../../odysseus_modules/sbgn/geometry.rkt")

(provide pd2af-alg1)

(define (-pd-keys el)
  (h- el 'name 'uoi 'components))

(define (-states el)
  (let ((variables? (not-empty? (intersect ($ state-variable-names defaults) (hash-keys el)))))
    (hash-union (hash 'variables? variables?) (hash-delete-all el ($ state-variable-names defaults)))))

(define-macro (pd-sexp- sexp . exprs)
  (define (build-pd-conditions-iter lst1 lst2)
    (cond
      ((or (empty? lst1) (empty? lst2)) `sexp)
      (else
        `(let ((,sexp (if ,(car lst1) (@@--- ,(car lst2) ,sexp) ,sexp)))
              ,(build-pd-conditions-iter (cdr lst1) (cdr lst2))
              ))))
  (match-let (((list (list removing-conditions removed-element-ids) ...) exprs))
    (build-pd-conditions-iter removing-conditions removed-element-ids)))

(define-macro (pd-ctx- context . exprs)
  (define (build-pd-conditions-iter lst1 lst2)
    (cond
      ((or (empty? lst1) (empty? lst2)) context)
      (else
        `(let ((,context (if ,(car lst1) (&&-- ,(car lst2) ,context) ,context)))
              ,(build-pd-conditions-iter (cdr lst1) (cdr lst2))
              ))))
  (match-let (((list (list removing-conditions removed-element-ids) ...) exprs))
    (build-pd-conditions-iter removing-conditions removed-element-ids)))

(define-macro (af-ctx+ af-context . exprs)
  (define (build-af-conditions-iter lst1 lst2)
    (cond
      ((or (empty? lst1) (empty? lst2)) af-context)
      (else
        `(let* ((af-element ,(car lst2))
                (af-element-id ($ id af-element))
                (pd-element (or (&& af-element-id pd-context) (hash)))
                (af-element-name (get-af-name pd-element pd-context))
                (af-class (to-af-class ($ class pd-element)))
                (af-element (hash-union
                              af-element
                              (hash 'name af-element-name 'class af-class)
                              (->> -states -pd-keys pd-element)))
                (af-element (if (BiologicalActivity? af-element)
                                    (hash-union (hash 'type (get-af-type ($ class pd-element))) af-element)
                                    af-element))
                (,af-context (if ,(car lst1) (&&++ af-element ,af-context) ,af-context))
                )
              ,(build-af-conditions-iter (cdr lst1) (cdr lst2))
              ))))
  (match-let (((list (list adding-conditions adding-element-hash) ...) exprs))
      (build-af-conditions-iter adding-conditions adding-element-hash)))

; extract node ids from logical gates expression for to initialize them in af-context
(define-catch (get-node-ids-from-combination combination context)
  (cond
    ((logical-combination? combination)
        (for/fold
          ((res empty))
          ((item combination))
          (cond
            ((node-id? item context) (pushr res item))
            ((logical-combination? item) (append res (get-node-ids-from-combination item context)))
            (else res)))) ; skip keywords and other non-ids
    ((scalar? combination) (get-node-ids-from-combination (list combination)))
    (else (filter (λ (item) (node-id? item context)) combination))))

; work with logical combinations on regulation
(define-catch (get-influence-sources substrate-ids regulator-ids #:influence-class (influence-class #f))
  (let* (
        (regulator-ids (if (list? regulator-ids) regulator-ids (list regulator-ids))) ; sometimes match for regulator-id, sometimes for regulator-ids
        (substrate-ids (if (list? substrate-ids) substrate-ids (list substrate-ids)))
        (regulator-logical-combination? (logical-combination? regulator-ids))
        (negative-influence? (and influence-class (equal? influence-class "negative influence")))
        (single-regulator? (one-element? regulator-ids))
        (several-regulators? (several-elements? regulator-ids)))
    (cond
      (regulator-logical-combination? `(,@substrate-ids ,regulator-ids))
      (negative-influence? `(,@regulator-ids ,@(map (λ (s) (list 'not s)) substrate-ids)))
      (single-regulator? `(,@substrate-ids ,(car regulator-ids)))
      (several-regulators? `(,@substrate-ids ,@regulator-ids))
      (else (error (format "unknown form for regulator: ~a" regulator-ids))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-catch (pd2af-alg1 sexp-0 af-context-0 pd-context-0)
  (define t (current-milliseconds))
  (define (set-class el new-class-name)
    (hash-union (hash 'class new-class-name) el))
  (let loop ((pd-context pd-context-0) (sexp sexp-0) (af-context af-context-0))
    (match sexp
      ; Hidden inhibition
      ((list-no-order
          `((,id2) ,p1-id (,id1))
          `((,id3) ,p2-id (,id4))
          `((,r1-id) ,c1-id (,p3-id))
          `((,r2-id) ,c2-id (,p4-id))
          _ ...)
        #:when  (and
                  (equal? id2 id3)
                  (equal? p1-id p3-id)
                  (equal? p2-id p4-id)
                  (inactive? id2 pd-context-0)
                  (< (n-as-substrate id2 pd-context) 3) ; id2 participates as substrate just in this pattern
                  (epn-id? id1 pd-context)
                  (epn-id? id2 pd-context)
                  (epn-id? id4 pd-context)
                  (not (ReversibleProcess? p1-id pd-context))
                  (not (ReversibleProcess? p2-id pd-context))
                  (not (exists-opposite-process? p1-id pd-context))
                  (not (exists-opposite-process? p2-id pd-context))
                  (same-names? (list id1 id2 id4) pd-context)
                  )
          (--- 1 r1-id r2-id id1 id2 id4)
          (let* (
                (c1 (&& c1-id pd-context))
                (c2 (&& c2-id pd-context))
                (c1-class (to-af-class ($ class c1)))
                (c2-class (to-af-class ($ class c2)))
                (ax? (active? id1 pd-context-0))
                (xa? (active? id4 pd-context-0))
                (ii? (and (inactive? id1 pd-context-0) (inactive? id4 pd-context-0)))
                )
            (loop
              (pd-ctx-
                pd-context
                (#t c1-id)
                (#t c2-id))
              (pd-sexp-
                sexp
                ((controlled-by-one? p1-id pd-context) p1-id)
                ((controlled-by-one? p2-id pd-context) p2-id)
                (#t c1-id)
                (#t c2-id))
              (af-ctx+
                af-context
                (ax? (hash 'id id1))
                (xa? (hash 'id id4))
                (#t (hash 'id r1-id))
                (#t (hash 'id r2-id))
                ((and ax?) (hash 'id (set-id) 'class c1-class 'sources (list r1-id) 'targets (list id1)))
                ((and ax?) (hash 'id (set-id) 'class (inverse-class c2-class) 'sources (list r2-id) 'targets (list id1)))
                ((and xa?) (hash 'id (set-id) 'class c2-class 'sources (list r2-id) 'targets (list id4)))
                ((and xa?) (hash 'id (set-id) 'class (inverse-class c1-class) 'sources (list r1-id) 'targets (list id4)))
                ))))
      ; Hidden inhibition with only one regulation
      ((list-no-order
          `((,id2) ,p1-id (,id1))
          `((,id3) ,p2-id (,id4))
          `((,r1-id) ,c1-id (,p3-id))
          _ ...)
        #:when  (and
                  (equal? id2 id3)
                  (equal? p1-id p3-id)
                  (inactive? id2 pd-context-0)
                  (< (n-as-substrate id2 pd-context) 3)
                  (epn-id? id1 pd-context)
                  (epn-id? id2 pd-context)
                  (epn-id? id4 pd-context)
                  (not (ReversibleProcess? p1-id pd-context))
                  (not (ReversibleProcess? p2-id pd-context))
                  (not (exists-opposite-process? p1-id pd-context))
                  (not (exists-opposite-process? p2-id pd-context))
                  (same-names? (list id1 id2 id4) pd-context)
                  )
          (--- 2 r1-id id1 id2 id4)
          (let* (
                (c1 (&& c1-id pd-context))
                (c1-class (to-af-class ($ class c1)))
                (ax? (active? id1 pd-context-0))
                (xa? (active? id4 pd-context-0)))
            (loop
              (pd-ctx-
                pd-context
                (#t c1-id))
              (pd-sexp-
                sexp
                ((controlled-by-one? p1-id pd-context) p1-id)
                (#t p2-id)
                (#t c1-id))
              (af-ctx+
                af-context
                (ax? (hash 'id id1))
                (xa? (hash 'id id4))
                (#t (hash 'id r1-id))
                ((and ax?) (hash 'id (set-id) 'class c1-class 'sources (list r1-id) 'targets (list id1)))
                ((and xa?) (hash 'id (set-id) 'class (inverse-class c1-class) 'sources (list r1-id) 'targets (list id4)))
                ))))
      ; Translocation with regulation
      ((list-no-order
          `((,id1) ,process-id (,id2))
          `((,regulator-id) ,control-id (,x-id))
          _ ...)
        #:when  (and
                  (equal? process-id x-id)
                  (node-id? id1 pd-context)
                  (node-id? id2 pd-context)
                  (equal? (get-signature-by-id id1 pd-context) (get-signature-by-id id2 pd-context))
                  (not-equal? (get-compartment-by-id id1 pd-context) (get-compartment-by-id id2 pd-context)))
          (--- 3 regulator-id id1 id2)
          (let* ((control (&& control-id pd-context))
                (control-class ($ class control)))
            (loop
              (pd-ctx-
                pd-context
                (#t control-id))
              (pd-sexp-
                sexp
                ((controlled-by-one? process-id pd-context) process-id)
                (#t control-id))
              (af-ctx+
                af-context
                (#t (hash 'id id1))
                (#t (hash 'id regulator-id))
                (#t (hash 'id control-id 'sources (list id1 regulator-id) 'targets (list id2)))
                (#t (hash 'id id2))))))
      ; Translocation
      ((list-no-order
          `((,id1) ,process-id (,id2))
          _ ...)
        #:when  (and
                  (epn-id? id1 pd-context)
                  (epn-id? id2 pd-context)
                  (equal? (get-signature-by-id id1 pd-context) (get-signature-by-id id2 pd-context))
                  (not-equal? (get-compartment-by-id id1 pd-context) (get-compartment-by-id id2 pd-context))
                  (not (port-id? id1 pd-context))
                  (not (port-id? id2 pd-context)))
          (--- 4 id1 id2)
          (loop
            pd-context
            (pd-sexp-
              sexp
              (#t process-id))
            (af-ctx+
              af-context
              (#t (hash 'id id1))
              (#t (hash 'id process-id 'class "positive influence" 'sources (list id1) 'targets (list id2)))
              (#t (hash 'id id2)))))
      ; Self-regulation
      ((list-no-order
          `((,regulator-id) ,control-id (,x-id))
          `((,substrate-id) ,process-id (,product-id))
          _ ...)
        #:when (and
                  (equal? x-id process-id)
                  (equal? regulator-id substrate-id))
          (--- 5 regulator-id substrate-id product-id)
          (let ((influence-class (to-af-class ($ class (&& control-id pd-context)))))
            (loop
              (pd-ctx-
                pd-context
                (#t control-id))
              (pd-sexp-
                sexp
                ((controlled-by-one? process-id pd-context) process-id))
              (af-ctx+
                af-context
                (#t (hash 'id substrate-id))
                (#t (hash 'id product-id))
                (#t (hash 'id regulator-id 'class influence-class 'sources (list substrate-id) 'targets (list product-id)))))))
      ; Association with regulation
      ((list-no-order
          `((,regulator-id) ,control-id (,x-id))
          `((,part-ids ...) ,process-id (,complex-id))
          _ ...)
        #:when (and
                  (equal? x-id process-id)
                  (Complex? complex-id pd-context)
                  (several-elements? part-ids)
                  (andmap (curryr epn-id? pd-context) part-ids))
          (--- 6 regulator-id part-ids complex-id)
          (let* ((part-ids (filter-not (λ (item-id) (ElementaryEPN? item-id pd-context)) part-ids))
                (influence-class (to-af-class ($ class (&& control-id pd-context))))
                (influence-sources (get-influence-sources part-ids regulator-id))
                (af-context (for/fold
                              ((af-context af-context))
                              ((part-id part-ids))
                              (af-ctx+
                                af-context
                                (#t (hash 'id part-id))))))
            (loop
              (pd-ctx-
                pd-context
                (#t control-id))
              (pd-sexp-
                sexp
                (#t control-id)
                ((controlled-by-one? process-id pd-context) process-id))
              (af-ctx+
                af-context
                (#t (hash 'id regulator-id))
                (#t (hash 'id regulator-id 'class influence-class 'sources influence-sources 'targets (list complex-id)))
                (#t (hash 'id complex-id))))))
      ; Association
      ((list-no-order
          `((,part-ids ...) ,process-id (,complex-id))
          _ ...)
        #:when (and
                  (Complex? complex-id pd-context)
                  (several-elements? part-ids)
                  (andmap (curryr epn-id? pd-context) part-ids))
          (--- 7 part-ids complex-id)
          (let* (
                ; (part-ids (filter-not (λ (item-id) (ElementaryEPN? item-id pd-context)) part-ids))
                (af-context (for/fold
                              ((af-context af-context))
                              ((part-id part-ids))
                              (af-ctx+
                                af-context
                                (#t (hash 'id part-id))))))
            (loop
              pd-context
              (pd-sexp-
                sexp
                (#t process-id))
              (af-ctx+
                af-context
                (#t (hash 'id process-id 'class "positive influence" 'sources part-ids 'targets (list complex-id)))
                (#t (hash 'id complex-id))))))
      ; Dissociation with regulation
      ((list-no-order
          `((,regulator-id) ,control-id (,x-id))
          `((,complex-id) ,process-id (,part-ids ...))
          _ ...)
        #:when (and
                  (equal? x-id process-id)
                  (Complex? complex-id pd-context)
                  (several-elements? part-ids)
                  (andmap (curryr epn-id? pd-context) part-ids))
          (--- 8 regulator-id complex-id part-ids)
          (let* ((part-ids (filter-not (λ (item-id) (ElementaryEPN? item-id pd-context)) part-ids))
                (influence-class (to-af-class ($ class (&& control-id pd-context))))
                (influence-sources (get-influence-sources part-ids regulator-id))
                (af-context (for/fold
                              ((af-context af-context))
                              ((part-id part-ids))
                              (af-ctx+
                                af-context
                                (#t (hash 'id part-id))))))
            (loop
              (pd-ctx-
                pd-context
                (#t control-id))
              (pd-sexp-
                sexp
                (#t control-id)
                ((controlled-by-one? process-id pd-context) process-id))
              (af-ctx+
                af-context
                (#t (hash 'id regulator-id))
                (#t (hash 'id regulator-id 'class influence-class 'sources (list complex-id) 'targets influence-sources ))
                (#t (hash 'id complex-id))))))
      ; Dissociation
      ((list-no-order
          `((,complex-id) ,process-id (,part-ids ...))
          _ ...)
        #:when (and
                  (Complex? complex-id pd-context)
                  (several-elements? part-ids)
                  (andmap (curryr epn-id? pd-context) part-ids))
          (--- 9 complex-id part-ids)
          (let* ((part-ids (filter-not (λ (item-id) (ElementaryEPN? item-id pd-context)) part-ids))
                (af-context (for/fold
                              ((af-context af-context))
                              ((part-id part-ids))
                              (af-ctx+
                                af-context
                                (#t (hash 'id part-id))))))
            (loop
              pd-context
              (pd-sexp-
                sexp
                (#t process-id))
              (af-ctx+
                af-context
                (#t (hash 'id process-id 'class "positive influence" 'sources (list complex-id) 'targets part-ids))
                (#t (hash 'id complex-id))))))
      ; Oligomerization with regulation
      ((list-no-order
          `((,regulator-ids ...) ,control-id (,x-id))
          `((,element-id) ,process-id (,multimer-id))
          _ ...)
        #:when (and
                  (equal? x-id process-id)
                  (or (node-ids? regulator-ids pd-context) (logical-combination? regulator-ids))
                  (Multimer? multimer-id pd-context)
                  (not (Multimer? element-id pd-context)))
          (--- 10 regulator-ids element-id multimer-id)
          (let* (
                (regulator-node-ids (get-node-ids-from-combination regulator-ids pd-context))
                ; add regulator nodes
                (af-context (for/fold
                          ((af-context af-context))
                          ((regulator-id regulator-node-ids))
                          (af-ctx+
                            af-context
                            (#t (hash 'id regulator-id))))))
          (loop
            (pd-ctx-
              pd-context
              (#t control-id))
            (pd-sexp-
              sexp
              ((controlled-by-one? process-id pd-context) process-id)
              (#t control-id))
            (af-ctx+
              af-context
              (#t (hash 'id element-id))
              (#t (hash 'id control-id 'sources (get-influence-sources element-id regulator-ids) 'targets (list multimer-id)))
              (#t (hash 'id multimer-id))))))
      ; Oligomerization
      ((list-no-order
          `((,element-id) ,process-id (,multimer-id))
          _ ...)
        #:when (and
                  (node-id? element-id pd-context)
                  (node-id? multimer-id pd-context)
                  (Multimer? multimer-id pd-context)
                  (not (Multimer? element-id pd-context)))
          (--- 11 element-id multimer-id)
          (loop
            pd-context
            (pd-sexp-
              sexp
              (#t process-id))
            (af-ctx+
              af-context
              (#t (hash 'id element-id))
              (#t (hash 'id process-id 'class "positive influence" 'sources (list element-id) 'targets (list multimer-id)))
              (#t (hash 'id multimer-id)))))
      ; Reversible process regulation
      ((list-no-order
          `((,regulator-ids ...) ,control-id (,x-id))
          `((,substrate-id) ,process-id (,product-id))
          _ ...)
        #:when (and
                  (equal? x-id process-id)
                  (node-id? substrate-id pd-context)
                  (node-id? product-id pd-context)
                  (or (node-ids? regulator-ids pd-context) (logical-combination? regulator-ids))
                  (node-id? process-id pd-context)
                  (ReversibleProcess? process-id pd-context))
          (--- 12 regulator-ids substrate-id product-id)
          (let* ((substrate-active? (active? substrate-id pd-context-0))
                (substrate-inactive? (not substrate-active?))
                (product-active? (active? product-id pd-context-0))
                (product-inactive? (not product-active?))
                (substrate-metabolite? (Metabolite? substrate-id pd-context))
                (product-metabolite? (Metabolite? product-id pd-context))
                (mm? (and substrate-metabolite? product-metabolite?))
                (aa? (and substrate-active? product-active? (not mm?)))
                (ia? (and substrate-inactive? product-active? (not mm?)))
                (ai? (and substrate-active? product-inactive? (not mm?)))
                (ii? (and substrate-inactive? product-inactive?))
                (s<x>? (s<x>? substrate-id pd-context-0))
                (p<x>? (p<x>? product-id pd-context-0))
                (influence-class (to-af-class ($ class (&& control-id pd-context))))
                ; (influence-class "unknown influence")
                (sources-to-product (cond
                                        ((NegativeInfluence? influence-class) `(,@regulator-ids (not ,substrate-id)))
                                        (else `(,@regulator-ids ,substrate-id))))
                (sources-to-substrate (cond
                                        ((NegativeInfluence? influence-class) `(,@regulator-ids (not ,product-id)))
                                        (else `(,@regulator-ids ,product-id))))
                (regulator-node-ids (get-node-ids-from-combination regulator-ids pd-context))
                ; add regulator nodes
                (af-context (for/fold
                          ((af-context af-context))
                          ((regulator-id regulator-node-ids))
                          (af-ctx+
                            af-context
                            (#t (hash 'id regulator-id)))))
                )
          (loop
            (pd-ctx-
              pd-context
              (#t control-id))
            (pd-sexp-
              sexp
              ((controlled-by-one? process-id pd-context) process-id)
              (#t control-id))
            (af-ctx+
              af-context
              (#t (hash 'id substrate-id))
              (#t (hash 'id product-id))
              ((or aa? ai? s<x>?) (hash 'id control-id 'class influence-class 'sources sources-to-substrate 'targets (list substrate-id)))
              ((or aa? ia? p<x>? (and (not mm?) ii?))
                  (cond
                    ; ((and s<x>? (equal? influence-class "negative influence"))
                    ;     (hash 'id control-id 'class influence-class 'sources `(,@regulator-ids (not ,substrate-id)) 'targets (list product-id)))
                    ; ((or s<x>?)
                    ;     (hash 'id control-id 'class influence-class 'sources `(,substrate-id ,@regulator-ids) 'targets (list product-id)))
                    (else
                        (hash 'id control-id 'class influence-class 'sources sources-to-product 'targets (list product-id)))))))))
      ; Basic signal pathway regulation
      ((list-no-order
          `((,regulator-ids ...) ,control-id (,x-id))
          `((,substrate-id) ,process-id (,product-id))
          _ ...)
        #:when (and
                  (equal? x-id process-id) ; (Macromolecule? (car substrate-id) pd-context) (Macromolecule? (car product-id) pd-context))
                  (node-id? substrate-id pd-context)
                  (node-id? product-id pd-context)
                  (or (node-ids? regulator-ids pd-context) (logical-combination? regulator-ids))
                  (node-id? process-id pd-context))
          (--- 13 regulator-ids substrate-id product-id)
          (let* (
                (substrate-active? (active? substrate-id pd-context-0))
                (substrate-inactive? (not substrate-active?))
                (product-active? (active? product-id pd-context-0))
                (product-inactive? (not product-active?))
                (substrate-metabolite? (Metabolite? substrate-id pd-context))
                (product-metabolite? (Metabolite? product-id pd-context))
                (s<s>? (substrate-and-substrate? substrate-id pd-context))
                ; substrates and products
                (p<p>? (product-and-product? product-id pd-context))
                (s<p>? (product-and-substrate? substrate-id pd-context))
                (p<s>? (product-and-substrate? product-id pd-context))
                (s<x>? (or s<p>? s<s>?))
                (p<x>? (or p<p>? p<s>?))
                (mm? (and substrate-metabolite? product-metabolite?))
                (aa? (and substrate-active? product-active? (not mm?)))
                (ia? (and substrate-inactive? product-active? (not mm?)))
                (ai? (and substrate-active? product-inactive? (not mm?)))
                (ii? (and substrate-inactive? product-inactive? (not mm?)))
                (ax? (or aa? ai?))
                (xa? (or aa? ia?))
                ; source-and-sinks
                (ssx? (SourceSink? substrate-id pd-context-0))
                (xss? (SourceSink? product-id pd-context-0))
                (control-class ($ class (&& control-id pd-context)))
                (influence-class (to-af-class control-class))
                (inverse-influence-class (inverse-class influence-class))
                (regulator-node-ids (get-node-ids-from-combination regulator-ids pd-context))
                ; add regulator nodes
                (af-context (for/fold
                          ((af-context af-context))
                          ((regulator-id regulator-node-ids))
                          (af-ctx+
                            af-context
                            (#t (hash 'id regulator-id)))))
                (regulator-ids-for-splicing (if (logical-combination? regulator-ids)
                                    (list regulator-ids) ; keep s-expression for logical combination
                                    regulator-ids))
                )
          (loop
            (pd-ctx-
              pd-context
              (#t control-id))
            (pd-sexp-
              sexp
              ((controlled-by-one? process-id pd-context) process-id)
              (#t control-id))
            (af-ctx+
              af-context
              ((or ax? xss?)
                    (hash 'id control-id 'class inverse-influence-class 'sources regulator-ids 'targets (list substrate-id)))
              ((or xa? ssx? mm? p<x>? (and ii? (not xss?)))
                  (let ((sources (cond
                                    (ssx? regulator-ids)
                                    ((and s<x>? (not aa?) (indexof? InhibitionArc control-class)) `(and (not ,substrate-id) ,@regulator-ids-for-splicing))
                                    ((and s<x>? (not aa?)) `(and ,substrate-id ,@regulator-ids-for-splicing))
                                    ((and mm? (indexof? InhibitionArc control-class)) `(and (not ,substrate-id) ,@regulator-ids-for-splicing))
                                    ((and mm?) `(and ,substrate-id ,@regulator-ids-for-splicing))
                                    (else regulator-ids))))
                    (hash 'id control-id 'class influence-class 'sources sources 'targets (list product-id))))
              ((or ax? mm? xss? s<x>?) (hash 'id substrate-id))
              ((or xa? mm? ii? ssx? p<x>?) (hash 'id product-id))))))
      ; Multiple substrates-products
      ((list-no-order
          `((,regulator-ids ...) ,control-id (,x-id))
          (list (list substrate-ids ...) process-id (list product-ids ...))
          _ ...)
        #:when (and
                  (equal? x-id process-id)
                  (node-ids? substrate-ids pd-context)
                  (node-ids? product-ids pd-context)
                  (or (node-ids? regulator-ids pd-context) (logical-combination? regulator-ids))
                  (node-id? process-id pd-context))
          (--- 14 regulator-ids substrate-ids product-ids)
          (let* (
                (active-substrate-ids (filter (curryr active? pd-context-0) substrate-ids))
                (active-product-ids (filter (curryr active? pd-context-0) product-ids))
                (metabolite-substrate-ids (filter (λ (x) (and (Metabolite? x pd-context) (not (CurrencyMetabolite? x pd-context)))) substrate-ids))
                (metabolite-product-ids (filter (λ (x) (and (Metabolite? x pd-context) (not (CurrencyMetabolite? x pd-context)))) product-ids))
                (influence-class (to-af-class ($ class (&& control-id pd-context))))
                (inverse-influence-class (inverse-class influence-class))
                (regulator-node-ids (get-node-ids-from-combination regulator-ids pd-context))
                ; add regulator nodes
                (af-context (for/fold
                              ((af-context af-context))
                              ((regulator-id regulator-node-ids))
                              (af-ctx+
                                af-context
                                (#t (hash 'id regulator-id)))))
                ; add non-currency metabolites
                (af-context (for/fold
                              ((af-context af-context))
                              ((metabolite-id (append metabolite-substrate-ids metabolite-product-ids)))
                              (af-ctx+
                                af-context
                                (#t (hash 'id metabolite-id)))))
                ; add substrates
                (af-context (for/fold
                              ((af-context af-context))
                              ((active-substrate-id active-substrate-ids))
                              (af-ctx+
                                af-context
                                (#t (hash 'id active-substrate-id)))))
                ; add inversed influence on substrates
                (af-context (af-ctx+
                              af-context
                              ((not-empty? active-substrate-ids) (hash 'id control-id 'class inverse-influence-class 'sources regulator-ids 'targets active-substrate-ids))))
                ; add products
                (af-context (for/fold
                              ((af-context af-context))
                              ((active-product-id active-product-ids))
                              (af-ctx+
                                af-context
                                (#t (hash 'id active-product-id)))))
                ; add influence on products
                (influence-sources (get-influence-sources metabolite-substrate-ids regulator-ids #:influence-class influence-class))
                (influence-targets (append active-product-ids metabolite-product-ids))
                (influence-targets (if (empty? influence-targets)
                                      (filter (curryr product-and-substrate? pd-context) (append substrate-ids product-ids))
                                      influence-targets))
                (af-context (af-ctx+
                              af-context
                              (#t (hash 'id control-id 'class influence-class 'sources influence-sources 'targets influence-targets))))
                )
            (loop
              (pd-ctx-
                pd-context
                (#t control-id))
              (pd-sexp-
                sexp
                ((controlled-by-one? process-id pd-context) process-id)
                (#t control-id))
              af-context)))
      ; Simple process between active elements - if nothing else matches
      ((list-no-order
          `((,ids1 ...) ,process-id (,ids2 ...))
          _ ...)
        #:when  (and
                  (or
                    (ormap (curryr active? pd-context-0) ids1)
                    (ormap (curryr active? pd-context-0) ids2)
                    (ormap (curryr s<x>? pd-context-0) ids1)
                    (ormap (curryr p<x>? pd-context-0) ids2))
                  (not (andmap (curryr SourceSink? pd-context-0) ids1))
                  (not (andmap (curryr SourceSink? pd-context-0) ids2))
                  (ormap (curryr epn-id? pd-context) ids2))
          (--- 15 ids1 ids2)
          (let* (
                ; add substrates
                (substrate-ids (filter-not (curryr CurrencyMetabolite? pd-context-0) ids1))
                (substrate-ids (filter-not (curryr SourceSink? pd-context-0) substrate-ids))
                (af-context (for/fold
                              ((af-context af-context))
                              ((substrate-id substrate-ids))
                              (af-ctx+
                                af-context
                                (#t (hash 'id substrate-id)))))
                ; add products
                (product-ids (filter-not (curryr CurrencyMetabolite? pd-context-0) ids2))
                (product-ids (filter-not (curryr SourceSink? pd-context-0) product-ids))
                (af-context (for/fold
                              ((af-context af-context))
                              ((product-id product-ids))
                              (af-ctx+
                                af-context
                                (#t (hash 'id product-id)))))
                ; add influence on products
                (af-context (af-ctx+
                              af-context
                              (#t (hash 'id process-id 'class "positive influence" 'sources substrate-ids 'targets product-ids)))))
            (loop
              pd-context
              (pd-sexp-
                sexp
                (#t process-id))
              (af-ctx+
                af-context))))
      ; if no more matches - exit
      (else
        ; (---- pd-context)
        ; (--- sexp)
        (--- (- (current-milliseconds) t) "ms")
        (values sexp af-context)))))
