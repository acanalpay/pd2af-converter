#lang racket

(require "../../odysseus/lib/_all.rkt" (for-syntax racket/syntax "../../odysseus/lib/_all.rkt"))
(require "../../odysseus/ontology/kvlist.rkt")
(require "common.rkt")
(require "types.rkt")
(require "context.rkt")
(require "sexp.rkt")
(require "sexp2ctx.rkt")
(require "../pd2af/translate.rkt")
(require rackunit)
(require compatibility/defmacro)

(provide (all-defined-out))

(define-catch (get-directed-graph context)
	(define (get-expression-signature-recur expr)
		(cond
			((logical-combination? expr) (map
																			get-expression-signature-recur
																			(if (equal? (car expr) 'and)
																			 			(cdr expr)
																						expr)))
			((LogicalPrefix? expr) expr)
			((scalar? expr) (get-signature-with-compartment (&& expr context) context))
			(else (errorf "wrong source expression form: ~a" expr))))
	(let ((result
						(for/fold
							((res (hash)))
							((arc (filter ActivityArc? context)))
							(let* (
										(source-ids (get-source-ids arc))
										(source-id (get-source-id arc))
										(target-ids (get-target-ids arc))
										(target-id (get-target-id arc))
										)
								(hash-union
									res
									(hash (list
													(cond
														((logical-combination? source-ids)
															(get-expression-signature-recur source-ids))
														((one-element? source-ids)
															(get-signature-with-compartment (&& source-id context) context))
														(else
															(sort
																(map (λ (x) (get-signature-with-compartment (&& x context) context)) source-ids)
																sort-by-names)))
													(cond
														((one-element? target-ids)
															(get-signature-with-compartment (&& target-id context) context))
														(else
															(sort
																(map (λ (x) (get-signature-with-compartment (&& x context) context)) target-ids)
																sort-by-names))))
												($ class arc)))))))
								result))

(define-catch (contexts-equal?
															context1 ; af context, translated by pd2af from pd sexp
															context2) ; sample af context
	; (--- "  ")
	; (---- context1)
	; (--- "  ")
	; (---- context2)
	; (--- "  ")
	(let* ((dg1 (get-directed-graph context1))
				(dg2 (get-directed-graph context2))
				(equal-hashes? (check-hash dg1 dg2 #:values-any-order? #t)))
		(when
			(not equal-hashes?)
			(--- "\n") (---- (hash-minus dg1 dg2 #:e same-elements?)) (--- "---") (---- (hash-minus dg2 dg1 #:e same-elements?)) (--- "\n"))
			; (--- "\n") (---- dg1) (--- "---") (---- dg2) (--- "\n"))
		equal-hashes?))

(define-macro (test-epic parameters . test-suites)
	`(let* ((parameters (kvlist->hash ',parameters))
					(epic-name (or ($ name parameters) "tests"))
					(knowledge-file (or ($ knowledge-file parameters) "_specification.tree"))
					(output-folder (or ($ output-folder parameters) ""))
					(test-report (make-parameter (hash))))
			,@test-suites
			(write-tab-tree
				knowledge-file
				(hash (hash 'id epic-name) (test-report)))))

(define-macro (test-suite parameters . test-cases)
	`(let* ((parameters (kvlist->hash ',parameters))
					(name (or ($ name parameters) ($ category parameters) "default"))
					(rule-number (+ 1 (length (hash-keys (test-report)))))
					(test-report-part (make-parameter (hash))))
			,@test-cases
			(test-report
				(hash-union
					(test-report)
					(hash (hash 'id name 'rule-number rule-number) (test-report-part))))))

(define-macro (test-case parameters context-pd context-af)
	`(let* ((parameters (kvlist->hash ',parameters))
						(name ($ name parameters))
						(_ (--- "testing:" name))
						(folder-path (string-append output-folder "/" name))
						(context-pd ,context-pd)
						(context-af ,context-af)
						(calculated-context-af (translate-pd (HG context-pd (context->sexp context-pd))))
						(passed? (contexts-equal? calculated-context-af context-af))
						(rule-number (+ 1 (length (hash-keys (test-report-part)))))
						(report (hash 'id name 'passed passed? 'rule-number rule-number))
						)
			(test-report-part (hash-union (test-report-part) (hash report (hash))))
			(write-file-to-dir #:dir folder-path #:file "pd.sbgn" (context->xml context-pd))
			(write-file-to-dir #:dir folder-path #:file "af.sbgn" (context->xml context-af #:map-type "activity flow"))
			passed?
			))

(define-macro (load-test-suite filename . load?)
	(cond
		((or (equal? load? '()) (true? (car load?)))
			(let* ((path (string-append "../tests/" filename))
						(file-contents (read-file path)))
				(read (open-input-string file-contents))))
		(else '(void))))

; '((("default" "PKA_cat-dimer-biological-activity-macromolecule") ("default" "PFKFB2-P-biological-activity-macromolecule")) . "positive influence")
;
; '((("default" "PKA_cat-biological-activity-macromolecule") ("default" "PFKFB2-P-biological-activity-macromolecule")) . "positive influence")
