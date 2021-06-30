#lang racket

(require "common.rkt")
(require "geometry.rkt")
(require "sexp.rkt")
(require "types.rkt")
(require "../../odysseus/lib/_all.rkt")

(provide (all-defined-out))

(define port-ids (make-parameter empty))
(define node-ids (make-parameter empty))
(define current-map-type (make-parameter #f))

(define (element-type element) (hash-ref element 'type #f))

(define (element-id element)
  ($ id element))

(define-catch (name-by-id id context)
  ($ name (&& id context)))

(define-catch (incoming-arcs? id context (d #f))
  (ormap
    (λ (element)
      (and
        (or* ProcessNode? Arc? ActivityArc? element)
        (indexof? ($ targets element) id)))
    context))

; total degree for a hyperarc
(define-catch (hyperarc-degree id context)
  (+ (hyperarc-incoming-degree id context) (hyperarc-outcoming-degree id context)))

(define-catch (hyperarc-incoming-degree id context)
  (length
    (uniques
      (filter
          (λ (element) (indexof? ($ sources element) id))
          context))))

(define-catch (hyperarc-outcoming-degree id context)
  (length
    (uniques
      (filter
          (λ (element) (indexof? ($ targets element) id))
          context))))

(define-catch (not-same-name-incoming-arcs? el-id context (d #f))
  (let* ((el (&& el-id context))
        (el-name ($ name el))
        (el-sources (filter (λ (e) (equal? ($ target e) el-id)) context))
        (el-sources-not-same-name (filter-not (λ (s) (equal? (name-by-id s context) el-name)) el-sources)))
    (ormap
      (λ (element)
        (and
          (or* ProcessNode? Arc? ActivityArc? element)
          (indexof? el-sources-not-same-name ($ id element))))
      context)))

(define-catch (same-names? names-list context)
  (let ((names (map
                  (λ (id) ($ name (&& id context)))
                  names-list)))
    (one-element? (uniques names))))

(define-catch (p-ctx context #:id (id #f) #:show (show #f))
  (let ((res (if id
                (filter
                  (λ (x) (equal? ($ id x) id))
                  context)
                context)))
    (if show
          (print-list (map (λ (x)
                              (map (λ (i)
                                      (hash-ref x i #f))
                                    show))
                              res))
          (print-list res))))

(define-catch (context-redirect old-ids new-id context)
  (for/fold
    ((res context))
    ((old-id old-ids))
    (for/list
      ((el res))
      (let (
            ; in the case we redirecxt ids in hypergraph
            (sources ($ sources el))
            (targets ($ targets el))
            ; in the case we redirect ids in the graph with SBGN elements (before making it a hypergraph)
            (source ($ source el))
            (target ($ target el)))
        (cond
          ((and sources (indexof? sources old-id))
            (hash-substitute el (cons 'sources (replace sources old-id new-id))))
          ((and targets (indexof? targets old-id))
            (hash-substitute el (cons 'targets (replace targets old-id new-id))))
          ((and source (equal? source old-id))
            (hash-substitute el (cons 'source new-id)))
          ((and target (equal? target old-id))
            (hash-substitute el (cons 'target new-id)))
          (else el))))))

;; context modification
(define-catch (remove-from-context-by-id context id)
  (let* ((context
          (for/fold
            ((res empty))
            ((el context))
            (let* ((sources ($ sources el))
                  (targets ($ targets el))
                  (in-sources? (and sources (indexof? sources id)))
                  (in-targets? (and targets (indexof? targets id))))
            (cond
              ((equal? ($ id el) id) res)
              (in-sources?
                (if (one-element? sources)
                  (remove-from-context-by-id res ($ id el))
                  (pushr res
                    (hash-union
                      (hash 'sources (exclude sources id))
                      el))))
              (in-targets?
                (if (one-element? targets)
                  (remove-from-context-by-id res ($ id el))
                  (pushr res
                    (hash-union
                      (hash 'targets (exclude targets id))
                      el))))
              (else (pushr res el))))))
        (context (filter-not (λ (el) (or
                                        (empty? ($ sources el))
                                        (empty? ($ targets el))))
                              context)))
    context))

  ; (filter-not (λ (x) (equal? ($ id x) id)) context))

; remove from context by id
(define (&&-- id context)
  (remove-from-context-by-id context id))

; add/substitute <hash-part> at element with <id>
(define-catch (&&-> id hash-part context)
  (let* ((element (&& id context)))
      (cond
        ((not element) context)
        (else
          (&&++ (hash-union hash-part element) (exclude context element))))))

; substitute old-id element to new-id element
(define-catch (&&<> old-id new-id context)
  ; (---- (filter (λ (el) (indexof? (list old-id new-id) ($ id el))) context))
  (let* (
        (old-element (&& old-id context))
        (old-sources ($ sources old-element))
        (old-targets ($ targets old-element))
        (new-element (&& new-id context))
        ; (_ (when (not new-element) (error (format "~a doesn't exist in the context") new-id)))
        (new-sources ($ sources new-element))
        (new-targets ($ targets new-element))
        (joint-sources (and old-sources new-sources (uniques (append old-sources new-sources))))
        (joint-targets (and old-targets new-targets (uniques (append old-targets new-targets))))
        ; exclude old element from context
        (context (exclude context old-element))
        ; if no new-id element exists in the context -- create a clone of old-id element, with new-id id.
        (context (if (not new-element)
                        (&&++
                          (hash-union (hash 'id new-id) old-element)
                          context)
                        context))
        ; in the case, element is Arc - redirect all links, that lead from el outside
        (context (cond
                    ((or (not-empty? joint-sources) (not-empty? joint-targets))
                      (&&-> new-id (hash 'sources joint-sources 'targets joint-targets) context))
                    (else
                      context)))
        ; substitute old-id to new-id in all Arc that income or outcome from the old element, if the element is a Node
        (context (for/fold
                  ((res empty))
                  ((e context))
                  (let* ((e-sources ($ sources e))
                        (e-sources (if e-sources
                                      (replace e-sources old-id new-id)
                                      #f))
                        (e-targets ($ targets e))
                        (e-targets (if e-targets
                                      (replace e-targets old-id new-id)
                                      #f)))
                    (cond
                      ((or e-sources e-targets) (pushr res (hash-union (hash 'sources e-sources 'targets e-targets) e)))
                      (else (pushr res e)))))))
    context))

; sources -arc- el -arc- targets
; return source and target neighbours for a given node
(define-catch (-&- el context)
  (let* ((el-id ($ id el))
        (arcs (filter
                (λ (x) (and ($ sources x) ($ targets x)))
                context))
        (in-arcs (filter
                    (λ (x) (indexof? ($ targets x) el-id))
                    arcs))
        (out-arcs (filter
                    (λ (x) (indexof? ($ sources x) el-id))
                    arcs))
        (sources (map
                    (λ (x)
                      (map
                        (λ (y) (&& y context))
                        ($ sources x)))
                    in-arcs))
        (targets (map
                    (λ (x)
                      (map
                        (λ (y) (&& y context))
                        ($ targets x)))
                    out-arcs))
        (sources (opt/uniques (flatten sources)))
        (target (opt/uniques (flatten targets))))
    ; (--- sources el-id target)
    (hash 'sources sources 'targets target)))

; add, if new
(define-catch (&&++ el context)
  (pushr-unique context el))

(define (&&>> . args)
  (let ((context (last args)))
    (for/fold
      ((res context))
      ((e (but-last args)))
      (&&++ e res))))

(define (reduce-ids context nodes-grouped)
  (for/fold
    ((res (list)))
    ((group nodes-grouped))
    (pushr res (get-element-by-id (car group) context))))

(define (only-connecting-arcs arcs sexp)
	(let ((existing-ids (opt/uniques (flatten sexp))))
		(filter
			(λ (arc) (indexof existing-ids ($ id arc)))
			arcs)))

(define-catch (get-arc-id source-id target-id glyphs)
  (let ((matched-glyphs
          (filter
            (λ (el)
                    (and
                      (indexof? ($ sources el) source-id)
                      (indexof? ($ targets el) target-id)))
            glyphs)))
    (if (empty? matched-glyphs)
      #f
      ($ id (car matched-glyphs)))))

(define-catch (get-context-parameter context parname (default #f))
	(let ((parameters
					(filter
						(λ (x) (equal? ($ class x) "parameters"))
						context)))
		(hash-ref (car parameters) parname default)))

(define-catch (what-kind-of-element el context)
	(let* ((context-sources (map
														(λ (x) ($ sources x))
														(filter
															(λ (y) ($ sources y))
															context)))
				(context-targets (map
													(λ (x) ($ targets x))
													(filter
														(λ (y) ($ targets y))
														context)))
				(id ($ id el)))
; #:source-end? (source-end? #f) #:target-end? (target-end? #f) #:mixed-end? (mixed-end #f) #:single-end? (single-end? #f))
		(hash
			'source-end?
			(and
				(for/or ((sources-set context-sources))
						(indexof? sources-set id)) ; somewhere it is source
				(for/and ((targets-set context-targets))
						(not (indexof? targets-set id)))) ; nowhere it is target
			'target-end?
			(and
				(for/or ((targets-set context-targets))
						(indexof? targets-set id)) ; somewhere it is target
				(for/and ((sources-set context-sources))
						(not (indexof? sources-set id)))) ; nowhere it is source
			'mixed-end?
			(for/or
        ((element-bunches (append context-targets context-sources)))
				(and
          (not-empty? (filter-not (λ (b-id) (ElementaryEPN? (&& b-id context))) element-bunches))
          (indexof? element-bunches id)))
			'single-end?
			(for/or
        ((element-bunches (append context-targets context-sources)))
				(and
          (indexof? element-bunches id)
          (one-element? element-bunches)))
			'one-action?
      (one-element?
        (filter
          (λ (e) (indexof? e id))
          (append context-targets context-sources)))
)))

;;;;;;;; AF
(define (create-af-element af-context element)
  (pushr af-context element))

(define-catch (add-af-element pd-context af-context element-id)
  (if (&& element-id af-context) ; if we have already added af element to af-context
    af-context
    (let* (
          (element (&& element-id pd-context))
          (af-element (cond
                                ((ProcessNode? element) (hash 'id element-id 'class "positive influence" 'sources ($ sources element) 'targets ($ targets element)))
                                ((Node? element) (hash 'id element-id  'class "biological activity" 'type ($ class element)  'compartment ($ compartment element) 'name ($ name element)
                                                        'x ($ x element) 'y ($ y element) 'w W 'h H
                                                        'uoi (or ($ uoi element) "")))
                                ((ModulationArc? element) (hash 'id element-id 'class (to-af-class ($ class element)) 'sources ($ sources element)  'targets ($ targets element)))
                                (else #f)))
          )
      (&&++ af-element af-context))))

(define (add-af-elements pd-context af-context . element-ids)
  (let ((element-ids (flatten element-ids)))
    (for/fold
      ((res af-context))
      ((element-id element-ids))
      (add-af-element pd-context res element-id))))

;
(define-catch (another-same-name+class-element? el af-context)
  (let* ((compartment ($ compartment el))
        (id ($ id el))
        (name ($ name el))
        (type (or ($ type el) ($ class el)))
        (uoi ($ uoi el))
        (nodes (exclude (filter ActivityNode? af-context) el)))
    (ormap
      (λ (x)
              (and
                (not (equal? ($ id x) id))
                (equal? ($ compartment x) compartment)
                (equal? ($ name x) name)
                (equal? (or ($ type x) ($ class x)) type)))
      nodes)))

;;; find signature
(define-catch (get-compartment el context)
	(->string ($ name (&& ($ compartment el) context))))

(define-catch (get-compartment-by-id el-id context)
  (let ((el (&& el-id context)))
    (get-compartment el context)))

(define-catch (get-signature-by-id el-id context)
  (get-signature (&& el-id context)))

(define-catch (get-signature el)
  (let* (
        (af? (AFElement? el))
        (name (get-node-name el))
        (class ($ class el))
        (type ($ type el))
        (type (if (and (not type) (BiologicalActivity? el))
                        "macromolecule"
                        type))
        (state-variables (if af?
                              empty
                              (get-state-variable-values el)))
        (res-list `(,name ,class ,type ,@state-variables))
        (res-list (cleanmap res-list))
        )
    (string-replace (implode res-list "-") " " "-")
    ))

(define-catch (get-signature-with-compartment el context)
	(let* (
        (compartment (get-compartment el context))
        (signature (get-signature el)))
		 (list compartment signature)))

;; detecting the status of element
(define (product? id context)
  (ormap
    (λ (element)
      (and (ProcessNode? element) (indexof? ($ targets element) id)))
    context))

(define (substrate? id context)
  (ormap
    (λ (element)
      (and (ProcessNode? element) (indexof? ($ sources element) id)))
    context))

(define (product-and-substrate? id context)
  (and
    (product? id context)
    (substrate? id context)
    (let ((products-for-this-element (map
                                        (λ (e) ($ targets e))
                                        (filter
                                            (λ (e)
                                                (and
                                                  (Process? e)
                                                  (not (ReversibleProcess? e))
                                                  (indexof? ($ sources e) id)))
                                            context)))

          (substrates-for-this-element (map
                                              (λ (e) ($ sources e))
                                              (filter
                                                  (λ (e)
                                                      (and
                                                        (Process? e)
                                                        (not (ReversibleProcess? e))
                                                        (indexof? ($ targets e) id)))
                                                  context))))
      (not-empty? (difference products-for-this-element substrates-for-this-element)))))

(define substrate-and-product? product-and-substrate?)

(define (substrate-and-substrate? id context)
  (several-elements?
    (filter
      (λ (element)
        (and
          (Process? element)
          (not (ReversibleProcess? element))
          (indexof? ($ sources element) id)))
      context)))

(define (product-and-product? id context)
  (several-elements?
    (filter
      (λ (element)
        (and
          (Process? element)
          (not (ReversibleProcess? element))
          (indexof? ($ targets element) id)))
      context)))

(define (substrate-in-reversible? id context)
  (ormap
    (λ (element)
      (and
        (ReversibleProcess? element)
        (or
          (indexof? ($ targets element) id)
          (indexof? ($ sources element) id))))
    context))

(define product-in-reversible? substrate-in-reversible?)

(define (regulator? id context)
  (ormap
    (λ (element)
      (and
        (ModulationArc? element)
        (indexof? ($ sources element) id)))
    context))

(define (multiple-regulator? id context)
  (< 1 (length
        (cleanmap
          (map
            (λ (element)
              (and (ModulationArc? element)
              (indexof? ($ sources element) id)))
            context)))))

(define (active-status? id context)
  (let ((state ($ state (&& id context))))
    (and state (re-matches? "^active.*" state))))

(define (inactive-status? id context)
  (let ((state ($ state (&& id context))))
    (and state (re-matches? "^inactive.*" state))))

(define (active? id context)
  (or
    (regulator? id context)
    (active-status? id context)))
    ; (and (product? id context) (substrate? id context) (not (substrate-in-reversible? id context))))

(define (inactive? id context)
  (or
    (not (active? id context))
    (inactive-status? id context)))

(define (s<x>? id context)
  (or
    (substrate-and-product? id context)
    (substrate-and-substrate? id context)))

(define (p<x>? id context)
  (or
    (substrate-and-product? id context)
    (product-and-product? id context)))

(define-catch (exists-opposite-process? process-id context)
  (let* ((process (&& process-id context))
        (source-ids ($ sources process))
        (target-ids ($ targets process))
        (opposite-processes (filter
                              (λ (el)
                                (and
                                  (intersect? source-ids ($ targets el))
                                  (intersect? target-ids ($ sources el))))
                              (filter Hyperarc? context))))
    (not-empty? opposite-processes)))

(define-catch (controls-over process-id context)
  (let ((control-ids (for/fold
                        ((res empty))
                        ((x (filter Arc? context)))
                        (cond
                          ((indexof? ($ targets x) process-id)
                            (cond
                              ((logical-combination? ($ sources x))
                                (pushr res ($ sources x)))
                              (else
                                (append res ($ sources x)))))
                          (else res)))))
    control-ids))

; process is controlled only by one element or one logical combination
(define-catch (controlled-by-one? process-id context)
  (one-element? (controls-over process-id context)))

(define-catch (controlled-by<2? process-id context)
  (let ((n (controls-over process-id context)))
    (or
      (empty? n)
      (one-element? n))))

(define-catch (only-reactant? id context)
  (ormap
    (λ (element)
      (and
        (ProcessNode? element)
        (or
          (and
            (indexof? ($ targets element) id)
            (not (several-elements? ($ targets element))))
          (and
            (indexof? ($ sources element) id)
            (not (several-elements? ($ sources element)))))))
    context))

; how many times id takes part in the scheme as a substrate
(define-catch (n-as-substrate id context)
  (length
    (filter
      (λ (element)
        (and
          (ProcessNode? element)
          (indexof? ($ sources element) id)))
      context)))

(define-catch (has-variables? el)
  (or
    ($ variables? el)
    (not-empty? (intersect ($ state-variable-names defaults) (hash-keys el)))))

(define (current-map-is-pd?)
  (equal? (current-map-type) "process description"))

(define (current-map-is-af?)
  (equal? (current-map-type) "activity flow"))

; add complex types, that require analysis of a context
(define (CurrencyMetabolite? el-id context)
  (and
    ((Class? CurrencyMetabolite (λ (el) (or ($ name el) ($ id el)))) el-id context)
    (not (only-reactant? el-id context))))

;; cache
(define (cache? item)
  (equal? ($ class item) "cache"))

(define (get-cache context)
  (let ((cache-items (filter cache? context)))
    (and (not-empty? cache-items) (car cache-items))))

(define-catch (push-cache cache context)
  (let ((context-without-cache (filter-not cache? context)))
    (pushr context-without-cache cache)))

(define (cache-port-ids context)
  (let* ((elements-with-port-ids (filter (λ (el) (or ($ in-id el) ($ out-id el))) context))
        (port-ids-lst (for/fold
                        ((res empty))
                        ((e elements-with-port-ids))
                        (append res (list ($ in-id e) ($ out-id e)))))
        (port-ids-lst (cleanmap port-ids-lst))
        (cache (get-cache context))
        (cache (hash-union (hash 'port-ids port-ids-lst) cache)))
    (push-cache cache context)))

(define (cache-node-ids context)
  (let* ((node-ids-lst (map (λ (el) ($ id el)) (filter Node? context)))
        (node-ids-lst (cleanmap node-ids-lst)) ; not sure this is necessary
        (cache (get-cache context))
        (cache (hash-union (hash 'node-ids node-ids-lst) cache)))
    (push-cache cache context)))

(define-catch (cache-frequently-used-data context)
  (->> cache-port-ids cache-node-ids
                              (pushr context (hash 'class "cache"))))

(define port-id?
  (memoize
      (λ (id context)
			   (let* ((elements-with-port-ids (filter (λ (el) (or ($ in-id el) ($ out-id el))) context))
    		        (port-ids (for/fold
    		                        ((res empty))
    		                        ((e elements-with-port-ids))
    		                        (append res (list ($ in-id e) ($ out-id e)))))
    		        (port-ids (cleanmap port-ids)))
				(indexof? port-ids id)))))

(define node-id?
  (memoize
    (λ (id context)
			(let ((node-ids (map
                        (λ (el) ($ id el))
                        (filter Node? context))))
		  	(indexof? node-ids id)))))

(define epn-id?
  (memoize
    (λ (id context)
			(let ((node-ids (map
                        (λ (el) ($ id el))
                        (filter EPN? context))))
		  	(indexof? node-ids id)))))

(define-catch (node-ids? ids context)
  (andmap (λ (id) (node-id? id context)) ids))

; visible name of element in SBGN-ML or root part of id:
(define-catch (build-name el #:uoi (uoi #f) #:name (name #f))
  (let* ((name (or name (get-node-name el))))
    (cond
      ((Multimer? el)
        (let* (
              (uoi (or uoi ($ uoi el)))
              (cardinality (if uoi
                                (get-first-group-match "N:([0-9]+)" uoi)
                                "2")) ; if no uoi for multimer, consider it as dimer
              (postfix-name (case cardinality
                              (("1") "")
                              (("2") "-dimer")
                              (("3") "-trimer")
                              (("4") "-tetramer")
                              (("5") "-pentamer")
                              (("6") "-hexomer")
                              (("7") "-heptamer")
                              (("8") "-octomer")
                              (("9") "-enneamer")
                              (("10") "-decamer")
                              (else "-multimer"))))
                  (string-append name postfix-name)))
      (else name))))

(define-catch (get-component-names el context)
  (cond
    ((not (Complex? el)) #f)
    (else
      (let* ((component-ids ($ components el))
            (component-names (map (λ (id)
                                    (cond
                                      ((Complex? id context) (get-component-names (&& id context) context))
                                      (else (get-node-name (&& id context)))))
                                  component-ids)))
        component-names))))

(define-catch (get-state-variable-values el)
  (and
    el
    (let* (
          (variable-names (minus (hash-keys el) reserved-keys))
          (variable-values (filter-not
                              empty-value?
                              (map (λ (x) (hash-ref el x)) variable-names))))
      variable-values)))

(define (state-variable-value? val)
  (let* ((s (append
              ($ state-variable-names defaults)
              ($ state-variable-values defaults)))
        (s (map ->string s)))
    (indexof? s (->string val))))

; pass also a context, because when considering complex with components, we need the context to be able to extract components names by their ids
(define-catch (get-af-name el context)
  (cond
    ((Complex? el)
      (let* (
            (all-component-ids ($ components el))
            (all-components (map (λ (id) (&& id context)) all-component-ids))
            ; (_ (--- all-component-ids all-components))
            (multimer-uoi (filter UnitOfInformation? all-components))
            (multimer-uoi (and (not-empty? multimer-uoi) (first multimer-uoi)))
            (components (filter-not UnitOfInformation? all-components))
            (component-names (map (λ (component) (get-af-name component context)) components))
            (component-names (cleanmap component-names))
            (component-names (sort component-names string<?))
            (name ($ name el))
            (result (cond
                      ((not ($ id el)) #f)
                      (name name)
                      ((and component-names (not-empty? component-names))
                          (implode component-names "-"))
                      (else
                          (->string ($ id el)))))
            (result (if (and (ComplexMultimer? el) multimer-uoi)
                        (build-name el #:uoi ($ name multimer-uoi) #:name result)
                        result)))
        result))
    (else
      (let* ((result (build-name el))
            (uoi ($ uoi el))
            (controlled-vocabulary (and uoi
                                        (cond
                                          ((re-matches? "ct:.+" uoi) (cadar (get-matches "ct:(.+)" uoi)))
                                          (else #f))))
            (result (or
                      (and result controlled-vocabulary (str result "-" controlled-vocabulary))
                      result))
            (state-variables (get-state-variable-values el))
            (result (cond
                      ((not ($ id el)) #f)
                      ((and result state-variables)
                          (implode
                            (cleanmap `(,result ,@state-variables))
                            "-"))
                      (result result)
                      (else
                          (->string ($ id el))))))
        result))))

; print elements with the given ids
(define (----id ids context)
  (let ((ids (if (list? ids) ids (list ids))))
  	(----
  		(filter (λ (el) (indexof? ids ($ id el))) context))))
