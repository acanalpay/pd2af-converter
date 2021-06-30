#lang racket

(require "../server/libs/odysseus/lib/_all.rkt" (for-syntax racket/syntax "../server/libs/odysseus/lib/_all.rkt"))
(require compatibility/defmacro)

(provide (all-defined-out))

(define-macro (test-case-Cii name pd-regulation af-influence)
	`(test-case
		(parameters ,(format-symbol "name:~a" name))
		(sbgn-pd-context
			(default:compartment
				(C macromolecule q:2-1)
				(A1 macromolecule name:A state:inactive1 q:1-2)
				(A2 macromolecule name:A state:inactive2 q:3-2)
				(A1 (process p) A2)
				(C ,pd-regulation p)))
		(sbgn-af-context
			(default:compartment
				(C biological-activity q:2-1)
				(A2 biological-activity name:A q:2-2)
				(C ,af-influence A2)))))

(define-macro (test-case-Cia name pd-regulation af-influence)
	`(test-case
		(parameters ,(format-symbol "name:~a" name))
		(sbgn-pd-context
			(default:compartment
				(C macromolecule q:2-1)
				(A1 macromolecule name:A state:inactive q:1-2)
				(A2 macromolecule name:A state:active q:3-2)
				(A1 (process p) A2)
				(C ,pd-regulation p)))
		(sbgn-af-context
			(default:compartment
				(C biological-activity q:2-1)
				(A2 biological-activity name:A q:2-2)
				(C ,af-influence A2)))))

(define-macro (test-case-Cai name pd-regulation af-influence)
	`(test-case
		(parameters ,(format-symbol "name:~a" name))
		(sbgn-pd-context
			(default:compartment
				(C macromolecule q:2-1)
				(A1 macromolecule name:A state:active q:1-2)
				(A2 macromolecule name:A state:inactive q:3-2)
				(A1 (process p) A2)
				(C ,pd-regulation p)))
		(sbgn-af-context
			(default:compartment
				(C biological-activity q:2-1)
				(A1 biological-activity name:A q:2-2)
				(C ,af-influence A1)))))

(define-macro (test-case-Caa name pd-regulation af-influence1 af-influence2)
	`(test-case
		(parameters ,(format-symbol "name:~a" name))
		(sbgn-pd-context
			(default:compartment
				(C macromolecule q:2-1)
				(A1 macromolecule name:A state:active1 q:1-2)
				(A2 macromolecule name:A state:active2 q:3-2)
				(A1 (process p) A2)
				(C ,pd-regulation p)))
		(sbgn-af-context
			(default:compartment
				(C biological-activity q:2-1)
				(A1 biological-activity name:A-active1 q:1-2)
				(A2 biological-activity name:A-active2 q:3-2)
				(C ,af-influence1 A1)
				(C ,af-influence2 A2)))))

(define-macro (test-case-Cmm name pd-regulation af-influence)
	`(test-case
		(parameters ,(format-symbol "name:~a" name))
		(sbgn-pd-context
			(default:compartment
				(C macromolecule q:2-1)
				(A simple-chemical q:1-2)
				(B simple-chemical q:3-2)
				(A (process p) B)
				(C ,pd-regulation p)))
		(sbgn-af-context
			(default:compartment
				(C biological-activity q:2-1)
				(A biological-activity type:simple-chemical q:1-2)
				(B biological-activity type:simple-chemical q:3-2)
				((A C) ,af-influence B)))))

(define-macro (test-case-multimer name num suffix)
	`(test-case
		(parameters ,(format-symbol "name:~a" name))
		(sbgn-pd-context
			(default:compartment
				(A1 macromolecule name:A q:1-1)
				(A2 macromolecule-multimer name:A ,(format-symbol "uoi:N:~a" num) q:1-3)
				(A1 (process p) A2)))
		(sbgn-af-context
			(default:compartment
				(A1 biological-activity name:A q:1-1)
				(A2 biological-activity ,(format-symbol "name:A-~a" suffix) q:1-3)
				(A1 positive-influence A2)))))
