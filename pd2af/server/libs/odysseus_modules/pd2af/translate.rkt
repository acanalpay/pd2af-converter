#lang racket

(require "xml2pd.rkt" "pd2pd.rkt" "pd2af.rkt" "af2af.rkt" "xml2xml.rkt")
(require "../../odysseus/lib/_all.rkt")
(require "../../odysseus_modules/sbgn/common.rkt")
(require "../../odysseus_modules/sbgn/types.rkt")
(require "../../odysseus_modules/sbgn/context.rkt")
(require "../../odysseus_modules/sbgn/sexp.rkt")
(require "../../odysseus_modules/sbgn/geometry.rkt")
(require "../../odysseus_modules/sbgn/sexp2ctx.rkt")
(require "../../odysseus_modules/sbgn/ctx2xml.rkt")
(require compatibility/defmacro)

(provide (all-defined-out))

(define (sort-by-names a b)
	(cond
		; if no compartment (compatment = #f):
		((or (not (first a)) (not (first b)))
			(string<? (second a) (second b)))
		; if the same compartment:
		((string=? (first a) (first b))
			(string<? (second a) (second b)))
		; order by compartments otherwise:
		(else
			(string<? (first a) (first b)))))

(define (pd-str->pd pd-str)
	(let* ((pd-form (read (open-input-string pd-str)))
				(pd-context (sexp->context pd-form))
				(pd-sexp (context->sexp pd-context))
				(pd (HG pd-context pd-sexp)))
		pd))

(define-catch (get-af-context af-str)
	(sexp->context (read (open-input-string af-str))))

(define-catch (translate-pd pd)
	(let* ((af-context (pd2af (HG-sexp pd) (HG-context pd))))
    (->>
      remove-self-loops
			strip-off-state-prefix-if-no-duplication
			af-context)))

(define-macro (sbgn-ml->af-xml sbgn_ml)
  `(let* (
          ; clean incoming XML from <annotation> tag and similar things
          (sbgn_ml (clean-sbgn-ml ,sbgn_ml))
          ; parse SBGN ML into context and sexp
          (pd (parse-pd-sbgn-ml sbgn_ml))
          ; translate SBGN PD and get result in the form of AF context
          (af-context (translate-pd pd))
          ; generate XML from the AF context
          (af-xml (context->xml af-context #:map-type "activity flow")))
      af-xml))
