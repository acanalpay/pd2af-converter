#lang racket

(require racket/syntax)
(require "../../odysseus/lib/_all.rkt")
(require "../../odysseus_modules/sbgn/common.rkt")
(require "../../odysseus_modules/sbgn/types.rkt")
(require "../../odysseus_modules/sbgn/context.rkt")
(require "../../odysseus_modules/sbgn/sexp.rkt")
(require "../../odysseus_modules/sbgn/geometry.rkt")

(provide (all-defined-out))

(define-catch (get-logical-chain-sexp gate context)
	(cond
		((LogicalGate? gate)
			(let* (
						(logical-prefix (->id ($ class gate)))
						(in-id ($ in-id gate))
						(out-id ($ out-id gate))
						(port-ids (cleanmap (list in-id out-id)))
						(source-ids (map
													(λ (logic-arc) ($ source logic-arc))
													(filter
														(λ (el) (indexof? port-ids ($ target el)))
														(filter LogicArc? context))))
						(sources (map (λ (id) (&& id context '(id in-id out-id))) source-ids))
						(epn-ids
										(map
											(λ (el) ($ id el))
											(filter EPN? sources)))
						(logical-gate-ids
										(map
											(λ (el) ($ id el))
											(filter LogicalGate? sources)))
						(expr (for/fold
											((res `(,logical-prefix ,@epn-ids)))
											((logical-gate-id logical-gate-ids))
											(let ((logical-gate (&& logical-gate-id context '(id in-id out-id))))
												(pushr res (get-logical-chain-sexp logical-gate context))))))
				expr))
		; if gate-id is not a logical gate then return just it
		(else (list ($ id gate)))))

(define-catch (get-reversible-process-nodes nodes arcs context)
	(filter
		(λ (node)
			(let* ((in-id ($ in-id node))
						(out-id ($ out-id node))
						(arcs-with-out-id-source (filter
																				(λ (arc) (equal? ($ source arc) out-id))
																				arcs))
						(arcs-with-in-id-source (filter
																				(λ (arc) (equal? ($ source arc) in-id))
																				arcs)))
				(and
          (not-empty? arcs-with-out-id-source)
          (not-empty? arcs-with-in-id-source))))
		nodes))

(define-catch (process-nodes-2-hyperarcs context)
	(define (get-port-ids node)
		(list ($ in-id node) ($ out-id node)))
	(define (make-hyperarc node in-node-ids out-node-ids context (class #f))
		(let* (
					(id ($ id node))
          (in-id ($ in-id node))
          (out-id ($ out-id node))
					(new-id (set-id #:prefix (id-prefix ($ class node)))) ; make ids for all hyperarcs in the context to look uniformly
					(hyperarc (hash-union (hash 'id new-id 'old-id id 'sources in-node-ids 'targets out-node-ids 'class (or class ($ class node))) node))
					(hyperarc (hash-delete-all hyperarc (list 'out-id 'in-id 'x 'y 'w 'h))) ; those are nodes that are transformed to hyperarcs, they don't have 'source and 'target
					(context (exclude context node)) ; exclude process node from context
					(context (context-redirect (list id in-id out-id) new-id context)) ; redirect all references in the context onto the 'new-id'
					(context (pushr context hyperarc))) ; add new hyperarc to context
			context))
  (let* (
				(arcs (filter FluxArc? context))
		    (nodes (filter (λ (x) (and
																(ProcessNode? x)
																(not (Hyperarc? x)))) ; if process is already a hyperarc - skip it
												context))
				(reversible-nodes (get-reversible-process-nodes nodes arcs context))
				(nodes (minus nodes reversible-nodes))
				; make hyperarcs from general process nodes
				(context
							(for/fold
								((res-context context))
						    ((node nodes))
						    (let* (
											(port-ids (get-port-ids node))
											(in-node-ids (flatten
																			(cleanmap
																				(map
																					(λ (arc) (if (indexof? port-ids ($ target arc))
																											($ source arc)
																											#f))
																					arcs))))
											(out-node-ids (flatten
																			(cleanmap
																				(map
																 					(λ (arc) (if (indexof? port-ids ($ source arc))
																											($ target arc)
																											#f))
																					arcs))))
											(res-context (make-hyperarc node in-node-ids out-node-ids res-context))
											)
									res-context)))
					; make hyperarcs from reversible process nodes
					(context
							(for/fold
								((res-context context))
								((node reversible-nodes))
								(let* (
											(in-id ($ in-id node))
											(out-id ($ out-id node))
											(in-node-ids (flatten
																			(cleanmap
																				(map
																					(λ (arc) (if (equal? ($ source arc) in-id)
																											($ target arc)
																											#f))
																					arcs))))
											(out-node-ids (flatten
																			(cleanmap
																				(map
																					(λ (arc) (if (equal? ($ source arc) out-id)
																											($ target arc)
																											#f))
																					arcs)))))
									(make-hyperarc node in-node-ids out-node-ids res-context "reversible process")))))
			(filter-not FluxArc? context))) ; remove all flux arcs from the context

(define-catch (modulations-2-hyperarcs context)
	(for/fold
		((res empty))
		((el context))
		(cond
			((ModulationArc? el)
				(let* (
              (id ($ id el))
              (new-id (set-id #:prefix (id-prefix ($ class el)))) ; make ids for all hyperarcs in the context to look uniformly
              ; no need to redirect ids in the context as no elements that point to the modulation or start from the modulation (in contrary to process nodes)
							(source-id ($ source el))
							(target-id ($ target el))
							(source (&& source-id context '(id in-id out-id)))
							(source-ids (cond
														((LogicalGate? source)
															(get-logical-chain-sexp source context))
														(else (list source-id))))
							(target (&& target-id context '(id in-id out-id)))
							(target-ids (list ($ id target))))
					(pushr
						res
						(hash-union
							(hash 'id new-id 'old-id id 'sources source-ids 'targets target-ids)
							(hash-delete-all el (list 'source 'target))))))
			(else (pushr res el)))))

; group regulators of the same modulation class through the OR gate
(define-catch (group-same-regulations context)
	(let loop ((new-context empty) (old-context context))
		(cond
			((empty? old-context) new-context)
			((Modulation? (car old-context))
				(let* ((modulation (car old-context))
							(modulation-class ($ class modulation))
							(old-context (cdr old-context))
							(modulation-id ($ id modulation))
							(target-process-id (get-target-id modulation))
							(source-ids ($ sources modulation))
							(modulations-over-the-same-process
								(filter
									(λ (item)
										(and
											(Modulation? item)
											(equal? (get-target-id item) target-process-id)
											(equal? (class->general modulation-class) (class->general ($ class item)))
											))
									old-context)))
					(cond
						((empty? modulations-over-the-same-process)
							(loop
								(pushr new-context modulation)
								old-context))
						(else
							(let* (
										(new-sources (for/fold
																		((res `(or ,@(sources-for-splicing source-ids))))
																		((r modulations-over-the-same-process))
																		(let* ((r-source-ids ($ sources r)))
																			`(,@res ,@(sources-for-splicing r-source-ids)))))
										(modulation (hash-union
																	(hash 'sources new-sources 'class (class->general modulation-class))
																	modulation))
										(new-context (pushr new-context modulation))
										(mod-samer-ids (map (λ (x) ($ id x)) modulations-over-the-same-process))
										(old-context-1 (filter-not
																		(λ (item)
																				(indexof? mod-samer-ids ($ id item)))
																		old-context)))
							(loop new-context old-context-1))))))
			(else
				(loop
					(pushr new-context (car old-context))
					(cdr old-context))))))

; (...) -> ((...) (...))
(define-catch (get-grouped-elements f elements (grouped-elements (list)) (results (list)))
	(cond
		((null? elements) grouped-elements)
		(else
			(let* ((element (car elements))
						(element-i (indexof results (f element)))
						(element-group (nth grouped-elements element-i))
						(element-group (pushr element-group ($ id element)))
						(results (if (= element-i 0) (pushr results (f element)) results))
						(grouped-elements (if (= element-i 0)
																(pushr grouped-elements element-group)
																(setn grouped-elements element-i element-group))))
				(get-grouped-elements f (cdr elements) grouped-elements results)))))

; fold nodes with same names and no controls between them
; (define-catch (fold-same-signatures pd)
; 	(define f-same (λ (x) (list ($ compartment x) ($ name x) ($ class x) ($ uoi x))))
; 	(let* (
;         (context0 (HG-context pd))
;         (sexp0 (HG-sexp pd))
; 				(nodes (filter Node? context0)) ; take only nodes, without arcs
; 				(nodes-grouped (get-grouped-elements f-same nodes))) ; merge with equal names (sameness defined by f-same)
; 		(let loop ((context context0) (sexp sexp0) (nodes-left nodes-grouped))
; 			(cond
; 				((empty? nodes-left) (HG context sexp))
; 				(else
; 					(let* ((node-group (car nodes-left))
; 								; 1 redirect references sources and targets in context and substitutes ids in sexp
; 								(pd (if (one-element? node-group)
; 											(HG context sexp)
; 											(HG
; 												(context-redirect node-group (car node-group) context)
; 												(subst-ids node-group (car node-group) sexp))))
; 								; 2 remove processes without controls between equal ids
; 								;; after ids substitutions some of process hyperarcs now can have nodes of same names on each end, in the case these hononyms are single or the same on both ends and process doesn't have incoming controls - we can remove this process hyperarc from the context completely
; 								(pd (HG
; 											; remove hyperarcs that point to the same source and target
; 											(filter-not
; 							 					(λ (el)
; 													(and
; 														(not (incoming-arcs? ($ id el) context))
; 														(hash-ref el 'sources #f)
; 														(hash-ref el 'targets #f)
; 														(equal? ($ sources el) ($ targets el))))
; 												(HG-context pd))
; 											(filter-not
; 												(λ (triplet)
; 													(match triplet
; 														(`(,id1 ,id2 ,id3)
; 																(and
; 																		(not (incoming-arcs? id2 context0))
; 																		(equal? id1 id3)))
; 														(else #f)))
; 												(HG-sexp pd)))))
; 					(loop (HG-context pd) (HG-sexp pd) (cdr nodes-left))))))))

; remove elements, that are parts of logical gates as we moved this information into the 'sources of hyperarcs
(define-catch (remove-non-multilines context)
	(filter-not (λ (el) (or* LogicArc? LogicalGate? el)) context))

(define-catch (merge-clones context)
	(define (cloned? item)
		($ clone? item))
	(define (get-all-clones-and-a-prototype el context)
		(let* ((name (get-node-name el))
					(result (filter (λ (item) (equal? (get-node-name item) name)) context))
					(result (sort result (λ (a b) (not ($ cloned? a)))))) ; a prototype is the first element in the list
			result))
	(let loop ((context-new empty) (context-rest context))
		(let* ((item (and (not-empty? context-rest) (car context-rest))))
			(cond
				((empty? context-rest) context-new)
				(($ cloned? item)
						(let* ((clones-family (get-all-clones-and-a-prototype item context-rest))
									(original-el (car clones-family))
									(original-id ($ id original-el))
									(clones (cdr clones-family))
									(clone-ids (map element-id clones))
									(context-rest (context-redirect clone-ids original-id context-rest))
									(context-rest (remove-from-context-by-id context-rest clone-ids))
									(context-rest (if (not-empty? context-rest) (cdr context-rest) context-rest))
									)
							(loop (pushr context-new original-el) context-rest)))
				(else
							(loop (pushr context-new item) (cdr context-rest)))))))
