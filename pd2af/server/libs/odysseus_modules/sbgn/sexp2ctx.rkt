#lang racket

(require racket/syntax)
(require compatibility/defmacro)
(require (for-syntax racket/match))
(require sxml)
(require "../../odysseus/lib/_all.rkt")
(require "common.rkt")
(require "types.rkt")
(require "context.rkt")
(require "sexp.rkt")
(require "ctx2xml.rkt")
(require "../pd2af/pd2pd.rkt")

(provide (all-defined-out))

; (define-namespace-anchor a)
; (define ns (make-parameter (namespace-anchor->namespace a)))

(void (set-id #:prefix "glyph-" #:reset #t)) ; clear count in the very begin

;; macros section
;; perhaps better to implement bricks through match forms together with other elements of pd-form
(define (process-lisp-sbgn map-type sbgn-form #:output-xml? (output-xml? #t))
  (let* (
        ; (_ (--- 2 sbgn-form))
        (sbgn-form (add-ids-to-unnamed-forms sbgn-form))
        ; (_ (--- 3 sbgn-form))
        (context (sexp->context sbgn-form))
        ; (_ (--- 4 context))
        (filepath (get-environment-parameter context 'filepath))
        (xml (context->xml context #:map-type map-type)))
    (if output-xml?
      xml
      context)))

(define-syntax (pd stx)
	(syntax-case stx ()
		((_ body ...)
      #'(process-lisp-sbgn "process description" body ...))))

(define-macro (sbgn-pd-context . body)
  (match body
    (`((file ,filepath))
        `(process-lisp-sbgn "process description" (read-data-from-file ,filepath ns) #:output-xml? #f))
    (`(,forms ...)
      `(process-lisp-sbgn "process description" ',forms #:output-xml? #f))))

(define-macro (sbgn-af-context . body)
  (match body
    (`((file ,filepath))
        `(process-lisp-sbgn "activity flow" (read-data-from-file ,filepath ns) #:output-xml? #f))
    (`(,forms ...)
      `(process-lisp-sbgn "activity flow" ',forms #:output-xml? #f))))

(define-syntax (af stx)
	(syntax-case stx ()
		((_ body ...)
      #'(process-lisp-sbgn "activity flow" body ...))))

; (define-syntax-rule (sbgn-af-context body ...)
;   (process-lisp-sbgn "activity flow" '(body ...) #:output-xml? #f))

;;;; pd-lisp -> pd-lisp
(define-catch (add-ids-to-unnamed-forms pd-form)
  (transform-list-recur
    pd-form
    (λ (x) (match x
              (`(source-and-sink ,parameters ...)
                `(,(set-id #:prefix "source-and-sink-") source-and-sink ,@parameters))
              (`(source ,parameters ...)
                `(,(set-id #:prefix "source-") source ,@parameters))
              (`(sink ,parameters ...)
                `(,(set-id #:prefix "sink-") sink ,@parameters))
              (else x)))))

;;;; pd-lisp -> context
(define-catch (add-environment-parameter context parameter-hash)
  (let* ((env-element (filter (λ (e) (hash-ref e 'env #f)) context))
        (env-element-parameters (if (empty? env-element) (hash) (hash-ref env-element 'env (hash)))))
      (if (empty? env-element)
        (pushr context (hash 'env parameter-hash))
        (pushr
          (exclude-all context env-element)
          (hash 'env (hash-union parameter-hash env-element-parameters))))))

(define-catch (get-environment-parameter context parameter)
  (let* ((env-element (filter (λ (e) (hash-ref e 'env #f)) context)))
    (if (empty? env-element)
        #f
        (let* ((env-element (first env-element))
              (env-parameters (hash-ref env-element 'env))
              (parameter-value (hash-ref env-parameters parameter #f)))
          parameter-value))))

(define-catch (new-id? context element-sexp)
  (and
    (list? element-sexp)
    (let ((ids (map (λ (x) ($ id x)) context))
          (id (car element-sexp)))
      (if (indexof? ids id)
        #f
        id))))

(define (get-node-id list-form)
  (cond
    ((scalar? list-form) list-form)
    (else (car list-form))))

(define-catch (->ids-combination list-form)
  (for/fold
    ((res empty))
    ((item (if (list? list-form) list-form (list list-form))))
    (cond
      ((empty? item) res)
      ((scalar? item) (pushr res item))
      ((list-form-of-PD-glyph? item) (pushr res (car item)))
      ((list-form-of-complex? list-form) (pushr res (car item)))
      ((logical-combination? item) (pushr res (make-logical-combination (logical-combination-header item) (->ids-combination (logical-combination-body item)))))
      (else res))))

(define-catch (read-class cls)
  (let ((cls (string-replace (->string cls) "-" " ")))
    (cond
      ((indexof? PDGlyph cls) cls) ;  for PD
      ((indexof? ActivityGlyph cls) cls) ; for AF
      (else #f))))

(define-catch (read-parameters pars)
  (local ((define (split-parameter-form p)
          (let ((couple (split (->string p) ":")))
            (hash (->symbol (first couple)) (implode (rest couple) ":")))))
    (let loop ((new-pars (hash)) (old-pars pars))
      (cond
        ((empty? old-pars) new-pars)
        ((equal? (length old-pars) 1) (hash-union new-pars (split-parameter-form (car old-pars))))
        (else
          (let* ((next-couple (lshift old-pars 2))
                (rest-pars (ltrim old-pars 2))
                (a (first next-couple))
                (b (second next-couple)))
            (if (string? b)
              (loop
                (hash-union new-pars (hash (rtrim-symbol a) b))
                rest-pars)
              (loop
                (hash-union new-pars (hash-union (split-parameter-form a) (split-parameter-form b)))
                rest-pars))))))))

(define-catch (sexp-item->hash-item element-sexp context)
  (cond
    ((list? element-sexp)
      (match element-sexp
        (`(,id ,complex (,inner-elements ...) ,parameters ...)
        #:when (Complex? complex)
          (cleanmap
            (append
              (list
                (hash-union
                  (hash 'id id 'class "complex")
                  (hash 'components (map get-node-id inner-elements))
                  (read-parameters parameters)))
              (map (λ (x)
                    (cond
                      ((scalar? x) #f)
                      (else (sexp-item->hash-item x context))))
                    inner-elements))))
        (`(,id ,class ,parameters ...)
          (hash-union
            (hash 'id id 'class (read-class class))
            (read-parameters parameters)))
        (else #f)))
    ((scalar? element-sexp) #f)
    (else #f)))

(define-catch (make-hash-hyperarc-element element-sexp (source-ids #f) (target-ids #f))
  (let (
        (base-hash
          (match element-sexp
            (`(,class ,id ,parameters ...)
              (hash-union
                (hash 'id id 'class (read-class class))
                (read-parameters parameters)))
            (class
                (hash 'id (set-id #:prefix "hyperarc-") 'class (read-class class)))
            (else #f))))
    (cond
      ((and base-hash source-ids target-ids)
          (hash-union base-hash (hash 'sources source-ids 'targets target-ids)))
      (base-hash base-hash)
      (else #f))))

(define (add-parameters-to-context id parameters-list-form context)
  (&&-> id (read-parameters parameters-list-form) context))

(define (add-node-to-context element context (compartment #f))
  (let ((new-elements (sexp-item->hash-item element context)))
    (cond
      ((logical-combination? element) context)
      ((list? new-elements) ; works just in the case when parsed list-form was a complex
        (let* ((complex (car new-elements))
              (complex-id ($ id complex))
              (components (cdr new-elements))
              (components (map (λ (x) (hash-union (hash-delete x 'compartment) (hash 'complex complex-id))) components))
              (context (pushr-unique context (cond
                                        (compartment (hash-union complex (hash 'compartment compartment)))
                                        (else complex)))))
          (append-unique context components)))
      ; single element
      (compartment
        (pushr-unique context (hash-union new-elements (hash 'compartment compartment))))
      (else
        (pushr-unique context new-elements)))))

(define-catch (add-hyperarc-element-to-context element source-form target-form context)
  (let* (
        (source-ids-form (->ids-combination source-form))
        (target-ids-form (->ids-combination target-form)))
    (pushr-unique context (make-hash-hyperarc-element element source-ids-form target-ids-form))))

(define (instantiate-element element-id context (compartment #f) #:id (id #f))
  (let* (
        (element (&& element-id context))
        (compartment? (Container? element))
        (new-element (hash-delete-all element '(def)))
        (new-id (if (or id compartment?) id (set-id #:prefix (str element-id "-"))))
        (new-element (hash-union (hash 'id new-id) new-element))
        (new-element (if compartment (hash-union (hash 'compartment compartment) new-element) new-element)))
    (pushr-unique context new-element)))

(define-catch (instantiate-element-sexp element-sexp context compartment)
  (let* ((element-id (get-node-id element-sexp))
        (element (&& element-id context))
        (instantiated-id (set-id #:prefix (str element-id "-"))))
    (cond
      ((Def? element)
        (add-parameters-to-context instantiated-id (cdr element-sexp) (instantiate-element element-id context compartment #:id instantiated-id )))
      (else
        context))))

; exclude ids and keywords (logical gates) from the first element of reaction/influence triplet, also suitable for the last element of triplet
(define-catch (get-element-definitions sexp-expression)
  (cond
    ((empty? sexp-expression) empty)
    (else
      (for/fold
        ((res empty))
        ((item sexp-expression))
          (cond
            ((scalar? item) res) ; exclude ids
            ((LogicalPrefix? item) res) ; exclude logical gates keywords
            ((logical-combination? item) (append res (get-element-definitions (logical-combination-body item))))
            ((empty? item) res) ; exclude '()-s if some
            (else (pushr res item)))))))

(define-catch (add-nodes-to-context element-sexps context (compartment #f))
  (cond
    ;; [a1 ...]
    ((scalar? element-sexps) (instantiate-element-sexp element-sexps context compartment)) ; if reference on the node, make sure it no longer contains 'def' parameter, so it can be drawn later
    ; A complex contains list of ids inside its list form. To avoid mixing this list with list form of an element, we make this special case, and check it before a general case
    ;; [(c1 complex (...) ...) ...]
    ((list-form-of-complex? element-sexps)
      (add-node-to-context element-sexps context compartment))
    ; ((logical-combination? element-sexps)
    ;   (extract-ids-from-logical-expression element-sexps context compartment))
    ;; [((e1 ...) e2 ...) ...]
    ((list>=2? element-sexps)
      (for/fold
        ((res-context context))
        ((element-sexp (get-element-definitions element-sexps)))
        (let* ((element-id (get-node-id element-sexp))
              (element-in-context (&& element-id res-context)))
          (cond
            ((new-id? context element-sexp)
              (add-node-to-context element-sexp res-context compartment))
            ((list? element-sexp)
              (cond
                ((Def? element-in-context)
                  (let ((instantiated-id (set-id #:prefix (str (car element-sexp) "-"))))
                    (add-parameters-to-context instantiated-id (cdr element-sexp) (instantiate-element element-id res-context compartment #:id instantiated-id )))) ; first, make copy of defed element and then add parameters to it
                (else
                  (add-parameters-to-context (car element-sexp) (cdr element-sexp) res-context)))) ; not new id but new parameters for already existed instantiated element
            ((scalar? element-sexp) (instantiate-element-sexp element-sexp context compartment))
            (else
              res-context)))))
    ;; [(e1 ...) ...]
    ((new-id? context element-sexps)
      (add-node-to-context element-sexps context compartment))
    ;; [(e1 e2 e3) ...]
    ((list? element-sexps)
      (for/fold
        ((res-context context))
        ((element-id element-sexps))
        (let* ((element (&& element-id context)))
          (if (and element (Def? element))
            (instantiate-element element-id res-context compartment) ; if references on the nodes, make sure they no longer contain 'def' parameter, so they can be drawn later; and also add coordinates
            res-context))))
    ;; ?
    (else context)))

(define-catch (read-result-file sbgn-form)
  (match sbgn-form
    ((list-no-order `(->file self.sbgn) _ ...)
      (let ((new-filename (format "~a.sbgn" (path->string (find-system-path 'run-file)))))
              (context (add-environment-parameter (context) (hash 'filepath new-filename)))))
    ((list-no-order `(->file ,filepath) _ ...)
      (context (add-environment-parameter (context) (hash 'filepath filepath))))
    (else (context))))
      ; (error "Don't know where to output the result. Add (->file <filepath>) form?"))))

(define-catch (read-defs sbgn-form)
  (let loop ((next-sbgn-form sbgn-form))
    (cond
      ((empty? next-sbgn-form) void)
      ((equal? (and (list? (car next-sbgn-form)) (caar next-sbgn-form)) 'defs)
        (set-id #:prefix "glyph-")
        (for
          ((d (cdar next-sbgn-form)))
          (match d
            (`(,id ,class ,parameters ...)
                (let ((parameters (append parameters '(def:#t))))
                  (context (append-unique
                              (context)
                              (add-nodes-to-context `(,id ,class ,@parameters) (context) #f)))))
            (else
              void))))
      (else
        (loop (cdr next-sbgn-form))))))

(define-catch (read-compartments sbgn-form)
  (let loop ((next-sbgn-form sbgn-form))
    (let* (
          (section-name (and (not (empty? next-sbgn-form)) (list? (car next-sbgn-form)) (caar next-sbgn-form)))
          (compartment-prefix (->string section-name))
          (section-parsed (split (->string section-name) ":"))
          (compartment (and (list? section-parsed) (> (length section-parsed) 1) (equal? (second section-parsed) "compartment")))
          (compartment-name (if compartment (first section-parsed) #f))
          (compartment-id (or compartment-name default-compartment-name))
          ; (compartment-id (if (equal? compartment-name default-compartment-name)
          ;                   default-compartment-name
          ;                   (set-id #:prefix (str compartment-name "-"))))
          )
      (cond
        ((empty? next-sbgn-form) void)
        (compartment
          (context
              (pushr
                (context)
                (hash 'id compartment-id 'class "compartment" 'name compartment-name)))
          (for
            ((triplet (cdar next-sbgn-form)))
            (let-values (((sources op targets)
                            (match triplet
                              (`(,sources
                                      ,op
                                          ,targets)
                                  #:when (or (ArcForm? op) (ActivityArcForm? op))
                                  (values sources op targets))
                              (`(,element ...)
                                  (values element #f #f))
                              (else
                                  (values #f #f #f)))))
                  (when sources
                            (context
                              (append-unique
                                (context)
                                (add-nodes-to-context sources (context) compartment-id))))
                  (when (and op sources targets)
                            (context
                              (append-unique
                                (context)
                                (add-hyperarc-element-to-context op sources targets (context)))))
                  (when targets
                            (context
                              (append-unique
                                (context)
                                (add-nodes-to-context targets (context) compartment-id))))))
          (loop (cdr next-sbgn-form)))
        (else
          (loop (cdr next-sbgn-form)))))))

(define-catch (sexp->context sexp)
  (set-id #:prefix "glyph-")
  (let ((res
          (->>
            group-same-regulations
              (parameterize ((context empty))
                    (read-defs sexp)
                    (read-compartments sexp)
                    (read-result-file sexp)
                    (context)
                    ))))
    ; (---- (filter Complex? res))
    res))
