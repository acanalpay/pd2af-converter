#lang racket

(require racket/syntax)
(require sxml)
(require "../../odysseus/lib/_all.rkt")
(require "../../odysseus_modules/sbgn/common.rkt")
(require "../../odysseus_modules/sbgn/types.rkt")
(require "../../odysseus_modules/sbgn/context.rkt")
(require "../../odysseus_modules/sbgn/sexp.rkt")
(require "../../odysseus_modules/sbgn/geometry.rkt")
(require "pd2pd.rkt")

(provide parse-pd-sbgn-ml)

; if we have class - this is a 'glyph'
(define (get-glyph-sexps sexp)
  ; ((txpath "//g:*[@class]" '((v . "v"))) sexp))
  ((txpath "/g:sbgn/g:map/g:*[@class]" '((g . "g"))) sexp)) ; g: = general namespace

(define-catch (get-compartment-id compartment)
  (cond
    ((scalar? compartment) (->id compartment))
    ((empty? compartment) #f)
    ((list? compartment) (->id (car compartment)))))

(define-catch (glyph-sexp->hash el)
  (match el
    ; logic gate
    (`(g:glyph ,(list-no-order '@ `(id ,id) `(class ,class) _ ...)
        (g:bbox ,(list-no-order '@ `(y ,y) `(x ,x) `(w ,w) `(h ,h)))
        (g:port ,(list-no-order '@ `(y ,_) `(x ,_) `(id ,port-id-1)))
        (g:port ,(list-no-order '@ `(y ,_) `(x ,_) `(id ,port-id-2))) ...) ; there is just one port for the logical gate in some cases, so get either empty or one-element list and provide first element to out-id at initialization
      #:when (LogicalPrefix? class)
          (let* ((port-id-1 (->id port-id-1))
                (port-id-2 (if (empty? port-id-2) #f (->id (car port-id-2)))))
            (list
              (hash
                'id (->id id)
                'name id
                'class class
                'in-id port-id-1
                'out-id port-id-2
                'x (->number x) 'y (->number y) 'w (->number w) 'h (->number h)))))

    ; source and sink
    (`(g:glyph ,(list-no-order '@ `(id ,id) `(class ,source-and-sink-class) `(compartmentRef ,compartment) ...) ; id is the first as sxml puts it on the first place after parsing, no matter which place id occupied in xml
        (g:bbox ,(list-no-order '@ `(y ,y) `(x ,x) `(w ,w) `(h ,h))))
      #:when (SourceSink? source-and-sink-class)
          (list
            (hash
              'id (->id id)
              'name id
              'compartment (get-compartment-id compartment)
              'class source-and-sink-class
              'x (->number x) 'y (->number y) 'w (->number w) 'h (->number h))))

    ; tag
    (`(g:glyph ,(list-no-order '@ `(id ,id) '(class "tag") _ ...)
        (g:label (@ (text ,name)))
        (g:bbox ,(list-no-order '@ `(y ,y) `(x ,x) `(w ,w) `(h ,h))))
          (list
            (hash
              'id (->id id)
              'name name
              'class "tag"
              'x (->number x) 'y (->number y) 'w (->number w) 'h (->number h))))

    ; complex:
    (`(g:glyph ,(list-no-order '@ `(class ,class) `(id ,id) `(compartmentRef ,compartment) ...)
        (g:label (@ (text ,name))) ...
        (g:bbox ,(list-no-order '@ `(y ,y) `(x ,x) `(w ,w) `(h ,h)))
        ,components ...)
        ; (g:glyph (@ (id ,component-ids) (class ,component-classes) ,_ ...)
        ;     (g:label (@ (text ,names-of-parts)))
        ;     ,_ ...) ...)
        #:when (indexof? '("complex" "complex multimer") class)
        (let* (
              (x (->number x)) (y (->number y)) (w (->number w))  (h (->number h))
              (x (+ x (/ w 2.0) (/ (- W) 2.0)))
              (y (+ y (/ h 2.0) (/ (- H) 2.0)))
              (w W)
              (h H))
            (let* ((components-context (flatten (map glyph-sexp->hash components))) ; get components of the complex recursevily
                  (component-ids (map (λ (item) ($ id item)) components-context))
                  (name (and name (not-empty? name) (car name))))
              ; join components context with a current complex item
              (pushr
                components-context
                (hash
                  'id (->id id)
                  'name name
                  'compartment (get-compartment-id compartment)
                  'components component-ids
                  'class class
                  'x x 'y y 'w w 'h h))))
    )

    ; compartment:
    (`(g:glyph ,(list-no-order '@ `(id ,id) '(class "compartment") _ ...)
        (g:label (@ (text ,name)) ,_ ...)
        (g:bbox ,(list-no-order '@ `(y ,y) `(x ,x) `(w ,w) `(h ,h))))
          (list
            (hash
              'id (->id id)
              'name name
              'class "compartment"
              'x (->number x) 'y (->number y) 'w (->number w) 'h (->number h))))

    ; process node:
    (`(g:glyph ,(list-no-order '@ `(id ,id) `(class ,class ...) _ ...)
        (g:clone) ... ; although this is wrong, but it can happen with some maps (generated in yEd?)
        (g:bbox ,(list-no-order '@ `(y ,y) `(x ,x) `(w ,w) `(h ,h)))
        (g:port (@ ,_ ... (id ,in-id)))
        (g:port (@ ,_ ... (id ,out-id))))
          (list
            (hash
              'id (->id id)
              'class (apply str class)
              'in-id (->id in-id) 'out-id (->id out-id)
              'x (->number x) 'y (->number y) 'w (->number w) 'h (->number h))))

    ; epn no state:
    (`(g:glyph ,(list-no-order '@ `(id ,id) `(class ,class ...) `(compartmentRef ,compartment) ...)
        (g:label (@ (text ,name)))
        ,clone ...
        (g:bbox ,(list-no-order '@ `(y ,y) `(x ,x) `(w ,w) `(h ,h))))
          (list
            (hash
              'id (->id id)
              'name name
              'compartment (get-compartment-id compartment)
              'class (apply str class)
              'cloned? (not-empty? clone)
              'x (->number x) 'y (->number y) 'w (->number w) 'h (->number h))))

    ; epn with state:
    (`(g:glyph ,(list-no-order '@ `(id ,id) `(class ,class ...) `(compartmentRef ,compartment) ...)
        ,clone ...
        (g:extension _ ...) ...
        (g:label (@ (text ,name)))
        (g:bbox ,(list-no-order '@ `(y ,y) `(x ,x) `(w ,w) `(h ,h)))
        ;; sometimes glyphs come without state name-values at all:
        (g:glyph (@ (id ,state-id-extra-1) (class "state variable"))
          (g:bbox ,(list-no-order '@ `(y ,y1_s_extra_1) `(x ,x1_s_extra_1) `(w ,w1_s_extra_1) `(h ,h1_s_extra_1))))
        ...
        ; normal state glyph:
        (g:glyph (@ (id ,state-id1) (class "state variable"))
          ,state-sexp1
          (g:bbox ,(list-no-order '@ `(y ,y1_s) `(x ,x1_s) `(w ,w1_s) `(h ,h1_s))))
        ...
        ; unit of information has floating position among state variables, so include patterns for state variables above and below
        (g:glyph (@ (id ,uoi-id) (class "unit of information"))
          (g:label (@ (text ,uoi)))
          (g:bbox ,(list-no-order '@ `(y ,y_uoi) `(x ,x_uoi) `(w ,w_uoi) `(h ,h_uoi))))
        ...
        ;; sometimes glyphs come without states at all:
        (g:glyph (@ (id ,state-id-extra-2) (class "state variable"))
          (g:bbox ,(list-no-order '@ `(y ,y1_s_extra_2) `(x ,x1_s_extra_2) `(w ,w1_s_extra_2) `(h ,h1_s_extra_2))))
        ...
        (g:glyph (@ (id ,state-id2) (class "state variable"))
          ,state-sexp2
          (g:bbox ,(list-no-order '@ `(y ,y2_s) `(x ,x2_s) `(w ,w2_s) `(h ,h2_s))))
        ...
        )
          (let* ((class (apply str class))
                (uoi (if (empty? uoi) #f (car uoi)))
                (states (append state-sexp1 state-sexp2))
                (states (and  (not-empty? states)
                              (for/fold
                                ((res (hash)))
                                ((i (append state-sexp1 state-sexp2)))
                                (match i
                                  (`(g:state ,(list-no-order '@ `(value ,value) `(variable ,variable)))
                                    (let ((variable (if (empty-string? variable) "_variable" variable)))
                                      (hash-union res (hash (->symbol variable) value))))
                                  (else res)))))
                (item
                      (hash
                        'id (->id id)
                        'uoi uoi
                        'class class
                        'name name
                        'compartment (get-compartment-id compartment)
                        'cloned? (not-empty? clone)
                        'x (->number x) 'y (->number y) 'w (->number w) 'h (->number h)))
                (item (if (and states (not (hash-empty? states)))
                        (hash-union item states)
                        item)))
            (list item)))

    ; arc:
    ((list-no-order 'g:arc (list-no-order '@ `(target ,target) `(source ,source) `(class ,class ...) _ ...)
        `(g:start (@ (y ,y1) (x ,x1)))
        `(g:end (@ (y ,y2) (x ,x2)))
        _ ...)
          (list
            (hash
              'id (set-id #:prefix "a")
              'class (apply str class)
              'source (->id source)
              'target (->id target)
              'x1 (->number x1) 'y1 (->number y1) 'x2 (->number x2) 'y2 (->number y2))))
    (else #f)))

(define (name-by-id id glyphs)
  ($ name (get-element-by-id id glyphs)))

(define-catch (parse-pd-sbgn-ml xml)
  (let* (
        (namespace (first (third (ssax:xml->sxml (open-input-string xml) '()))))
        (namespace (string-replace (symbol->string namespace) ":sbgn" ""))
        (sxml (ssax:xml->sxml (open-input-string xml) `((g . ,namespace))))
        (glyph-sexps (get-glyph-sexps sxml))
        (context (filter PDGlyph?
                    (filter true?
                      (flatten
                        (map glyph-sexp->hash glyph-sexps)))))
        (context (pushr context (hash 'class "parameters" 'namespace namespace)))
        (context (->>
                    group-same-regulations
                    remove-non-multilines
                    modulations-2-hyperarcs
                    process-nodes-2-hyperarcs
                      context))
        ; (_ (--- (length context)))
        (context (merge-clones context))
        ; (_ (--- (length context)))
        (pd-sexp `(sbgn-pd))
        (pd-sexp (add-interactions pd-sexp context))
        )
    (HG context pd-sexp)))
