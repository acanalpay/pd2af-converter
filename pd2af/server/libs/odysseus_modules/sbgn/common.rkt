#lang racket

(require "../../odysseus/lib/_all.rkt")
(require compatibility/defmacro)

(provide (all-defined-out))

(define empty-values '("" "<f>" "none" "empty"))

(define (empty-value? x)
  (indexof? empty-values x))

(define defaults (hash
                    'W 1000 'H 800
                    'el-w 110 'el-h 50
                    'sink-w 25 'sink-h 25
                    'gate-w 25 'gate-h 25 'gate-margin 5
                    'process-w 15 'process-h 15
                    'compartment-margin-x 30 'compartment-margin-y 30
                    'compartment-w 100 'compartment-h 100
                    'square-w 200 'square-h 100 ; side sizes of a q-grid
                    'default-x 0 'default-y 0 ; x,y of element if not any hints (q parameter, location in the complex etc.)
                    'component-vertical-gap 15
                    'uoi-w 40 'uoi-h 10
                    ; 'state-variable-positions '(70 10 40 100)
                    'state-variable-positions '(70 10 40 25 55)
                    'state-position 'top
                    'state-variable-names '(P p p1 p2 Me me Ub ub Ac ac SUMO Sumo sumo state state1 state2 state3 state4 _variable)
                    'state-variable-values '(PP ATP Ca active active1 active2 inactive inactive1 inactive2)
                    'empty-values empty-values
                    'state-variable-d 15 'state-variable-w 30 'state-variable-h 15
                    ))

(define reserved-keys '(id old-id in-id out-id
                        name
                        compartment class type
                        sources targets
                        cardinality
                        uoi
                        q qx qy x y w h
                        x1 x2 y1 y2
                        in-x in-y out-x out-y
                        complex components
                        def
                        variables? cloned? active?))

; custom geometry parameters for simple diagrams
(define simple-generation (hash 'el-w 80 'el-h 40 'process-w 24 'process-h 24 'sink-w 50 'sink-h 50))
(define defaults-1 (hash-union simple-generation defaults))

(define default-compartment-name "default")

(define context (make-parameter empty))
(define context-compartment (make-parameter empty))

(define (HG context sexp)
  (hash 'context context 'sexp sexp))

(define (HG-context pd)
  (hash-ref pd 'context))

(define (HG-sexp pd)
  (hash-ref pd 'sexp))

(define (->id id)
  (if (re-matches? "^\\d+$" (->string id))
    (->symbol (str "id" (->string id)))
    (->symbol id)))

(define purify-string (change-text (list
                                      (cons "\"" "")
                                      (cons "&quote;" ""))))

(define get-element-by-id
  (memoize
    (λ (id context)
      (let ((matched-elements
              (filter
                (λ (el) (equal? ($ id el) id))
                context)))
        (if (empty? matched-elements)
          #f
          (car matched-elements))))))

; find item in the context that matches given value 1) by id 2) by any of the given keys
(define (&& id context (keys #f))
  (cond
    ((or (not keys) (empty? keys))
      (get-element-by-id id context))
    ((scalar? keys)
      (let ((res (filter
                    (λ (el) (equal? (hash-ref el keys #f) id))
                    context)))
        (if (empty? res) #f (car res))))
    ((list? keys)
      (or (&& id context (car keys)) (&& id context (cdr keys))))
    (else #f)))

(define (get-pd-filename path (ext ".pd.sbgn"))
  (let* (
  				(filename (string-replace path "\\" "/"))
  				(filename (string-split filename "/"))
  				(filename (last filename))
  				(filename (string-split filename "."))
  				(filename (first filename))
  				(filename (str filename ext)))
    filename))

(define (class-name class)
  class)
  ; (->symbol (string-replace (->string class) " " "_")))

(define (id-prefix id)
  (format "~a-" (string-replace (->string id) " " "-")))

; orders sxml lists by tagname
(define (order-by-tag tags-order sxml)
	(sort sxml
		(λ (a b)
      (and (list? a) (list? b)
  			(let* ((tag-a (car a))
  						(tag-b (car b))
  						(pos-a (indexof tags-order tag-a))
  						(pos-b (indexof tags-order tag-b)))
  				(< pos-a pos-b))))))

(define-catch (ontology-uoi? uoi)
	(and uoi
			(re-matches? ":|ct:|mt:" uoi)))

(define-catch (get-node-name el)
  (let* ((result
            (cond
              ((not ($ id el)) #f)
              ((not el) #f)
              ((hash-empty? el) #f)
              ((not ($ name el)) (->string ($ id el)))
              ((scalar? ($ name el)) (->string ($ name el)))
              ((not (list? ($ name el))) (errorf "wrong type for element's name: ~a (~a)" ($ name el) (type ($ name el))))
              ((empty? ($ name el)) ($ id el))
              (else (implode ($ name el) "-"))))
          (result (and result
                      (purify-string
                        (first
                          (string-split
                            result
                            "__")))))) ; strip off compartment appendix
    result))

(define (get-target-id el)
  (or
    (and ($ targets el) (car ($ targets el)))
    ($ target el)))

(define (get-target-ids el)
  (or
    ($ targets el)
    (and ($ target el) (list ($ target el)))
    empty))

(define (get-target el context)
  (&& (get-target-id el) context))

(define (get-source-id el)
  (or
    (and ($ sources el) (car ($ sources el)))
    ($ source el)))

(define (get-source-ids el)
  (or
    ($ sources el)
    (and ($ source el) (list ($ source el)))
    empty))

(define (get-context cs)
  ($ pd-context cs))

(define (get-sexp cs)
  ($ pd-sexp cs))

; frequently used for debugging output
(define (map-id context)
  (map (λ (x) ($ id x)) context))
