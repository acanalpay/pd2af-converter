#lang racket

(require compatibility/defmacro)
(require "../../odysseus/lib/_all.rkt")
(require "common.rkt")

(provide (all-defined-out))

(define Context (list "namespace"))

; element
;   glyph
;     tag
;     equivalence_arc
;     compartment
;     node
;       process_node
;       empty_set
;       entity_pool_node
;     arc
;       flux_arc
;       modulation_arc
;       logic_arc
;       hyperarc

(define-catch (Class? aset (f (λ (x) ($ class x))))
  (λ args
    (match args
      ((list el) (cond
                    ((hash? el) (indexof? aset (f el)))
                    (else (indexof? aset (string-replace (->string el) "-" " ")))))
      ((list el-id context) (indexof? aset (f (&& el-id context))))
      (else #f))))

(define (class->string class)
  (string-replace (->string class) "-" " "))

(define (ClassByForm? aset)
  (λ (form)
    (cond
      ((scalar? form)
        ; (--- form "--" aset "--" (class->string form) "--" (indexof? aset (class->string form)))
        (indexof? aset (class->string form)))
      ((list? form)
        ; (--- form "--" aset "--" (map class->string form) "--" (not (empty? (intersect aset (map class->string form)))))
        (not (empty? (intersect aset (map class->string form)))))
      (else #f))))

;;;;;;;;;;;;;;;;;;;; PD glyphs by classes
(define Container (list "compartment"))
    (define Container? (Class? Container))
(define ReferenceNode (list "tag"))

(define ElementaryEPN (list "unspecified entity" "simple chemical" "simple chemical multimer" "source and sink" "source" "sink"))
    (define ElementaryEPN? (Class? ElementaryEPN))
(define Metabolite (list "simple chemical" "simple chemical multimer"))
    (define Metabolite? (Class? Metabolite))
(define CurrencyMetabolite (list "ATP" "ADP" "GTP" "GDP" "NAD+" "NADH" "e-" "H+" "OH-" "Pi" "O" "HCO3-"))
(define NonElementaryEPN (list "macromolecule" "macromolecule multimer" "nucleic acid feature" "nucleic acid feature multimer" "perturbing agent" "complex" "complex multimer")) (define NonElementaryEPN? (Class? NonElementaryEPN))
(define Complex (list "complex" "complex multimer"))
    (define Complex? (Class? Complex))
(define SimpleChemicalMultimer (list "simple chemical multimer"))
    (define SimpleChemicalMultimer? (Class? SimpleChemicalMultimer))
(define MacromoleculeMultimer (list "macromolecule multimer"))
    (define MacromoleculeMultimer? (Class? MacromoleculeMultimer))
(define ComplexMultimer (list "complex multimer"))
    (define ComplexMultimer? (Class? ComplexMultimer))
(define Multimer (append ComplexMultimer MacromoleculeMultimer SimpleChemicalMultimer))
    (define Multimer? (Class? Multimer))
(define EPN (append ElementaryEPN NonElementaryEPN))
    (define EPN? (Class? EPN)) ; Entity Pool Node
(define NamedEPN (minus EPN '("source and sink" "source" "sink" "complex" "complex multimer")))
    (define NamedEPN? (Class? NamedEPN)) ; Entity Pool Node
(define UnnamedEPN '("source and sink" "soucre" "sink" "complex" "complex multimer"))
    (define UnnamedEPN? (Class? UnnamedEPN)) ; Entity Pool Node
(define UnitOfInformation '("unit of information"))
    (define UnitOfInformation? (Class? UnitOfInformation))
(define SourceSink '("source and sink" "source" "sink"))
    (define SourceSink? (Class? SourceSink))
(define EmptySet (list "empty set"))
(define ProcessNode (list "process" "reversible process" "omitted process" "uncertain process" "association" "dissociation"))
  (define Process ProcessNode)
  (define ProcessNode? (Class? ProcessNode))
  (define Process? ProcessNode?)
(define LogicalOperator (list "and" "or" "not"))
    (define LogicalOperator? (Class? LogicalOperator))
    (define LogicalGate? LogicalOperator?)
    (define (LogicalPrefix? prefix) (indexof? LogicalOperator (->string prefix)))
(define PDNode (append EPN EmptySet ProcessNode LogicalOperator))
    (define PDNode? (Class? PDNode))

(define FluxArc (list "consumption" "production"))
    (define FluxArc? (Class? FluxArc))
(define InhibitionArc (list "inhibition"))
    (define InhibitionArc? (Class? InhibitionArc))
(define ModulationArc (list "modulation" "stimulation" "catalysis" "inhibition" "necessary stimulation"))
  (define ModulationArc? (Class? ModulationArc))
  (define Modulation ModulationArc)
  (define Modulation? ModulationArc?)
(define LogicArc (list "logic arc"))
    (define LogicArc? (Class? LogicArc))
(define EquivalenceArc empty)
(define ReversibleProcess (list "reversible process"))
    (define ReversibleProcess? (Class? ReversibleProcess))
(define GeneralProcess (minus ProcessNode ReversibleProcess))
    (define GeneralProcess? (Class? GeneralProcess))
(define (Hyperarc? . args)
  (let ((el
          (match args
            ((list el) el)
            ((list el-id context) (&& el-id context))
            (else (error (format "Wrong arguments number for Hyperarc?: ~a" args))))))
    (and
      (indexof? (hash-keys el) 'sources)
      (indexof? (hash-keys el) 'targets)
      (or* ProcessNode? ModulationArc? ActivityArc? el))))
(define Arc (append FluxArc ModulationArc LogicArc EquivalenceArc))
    (define Arc? (Class? Arc))
(define ArcForm (append ProcessNode ModulationArc))
    (define ArcForm? (ClassByForm? ArcForm))

(define PDGlyph (append ReferenceNode EquivalenceArc PDNode Container Arc UnitOfInformation))
  (define PDGlyph? (Class? PDGlyph))

;;;;;;;;;;;;;;;;;;;; AF glyphs by classes
(define ActivityNode (list "biological activity" "phenotype" "perturbation"))
  (define ActivityNode? (Class? ActivityNode))
(define NegativeInfluence (list "negative influence"))
    (define NegativeInfluence? (Class? NegativeInfluence))
(define ActivityTypes (list "unspecified entity" "simple chemical" "macromolecule" "nucleic acid feature" "complex" "perturbation"))
(define ActivityDefaultTypes (list "macromolecule"))
(define ActivityArc (list "unknown influence" "positive influence" "negative influence" "necessary stimulation" "logic arc" "equivalence arc"))
    (define ActivityArc? (Class? ActivityArc))
    (define ActivityArcForm? (ClassByForm? ActivityArc))
(define ActivityLogicalOperator (list "and" "or" "not"))
    (define ActivityLogicalOperator? (Class? ActivityLogicalOperator))
(define ActivityGlyph (append ActivityNode ActivityArc ActivityLogicalOperator))
    (define ActivityGlyph? (Class? ActivityGlyph))

;;;;;;;;;;;;;;;;;;;; Categories of glyphs
(define Node (append EPN EmptySet ProcessNode LogicalOperator ActivityNode))
    (define Node? (Class? Node))

(define (InternalEPN? el) ($ complex el))
(define (Def? el) ($ def el))
(define (Env? el) ($ env el))
(define NonDefaultEPN (minus EPN '("macromolecule" "macromolecule multimer" "source and sink" "source" "sink"))) (define NonDefaultEPN? (Class? NonDefaultEPN)) ; Entity Pool Node

(define (type-checker classname)
  (λ args
    (match args
      ((list element-id context)
          (let ((el (&& element-id context)))
            ; (---
            ;   classname
            ;   (equal? classname (hash-ref (or (&& element-id context) (hash)) 'class #f))
            ;   ($ class el)
            ;   )
            (equal? classname (hash-ref (or (&& element-id context) (hash)) 'class #f))) )
      ((list element)
          (equal? classname (hash-ref element 'class #f))))))

(define Macromolecule? (type-checker "macromolecule"))
(define BiologicalActivity? (type-checker "biological activity"))
(define AFElement? ActivityNode?)

;;;;;;;;;;;;;;;;;;;;; Types mappings
(define PD-to-AF-types (hash
                          "unspecified entity" "unspecified entity"
                          "simple chemical" "simple chemical"
                          "simple chemical multimer" "simple chemical"
                          "macromolecule" "macromolecule"
                          "macromolecule multimer" "macromolecule"
                          "nucleic acid feature" "nucleic acid feature"
                          "complex" "complex"
                          "complex multimer" "complex"
                          "perturbation" "perturbation"))

(define (get-af-type class)
  (hash-ref PD-to-AF-types class "macromolecule"))

(define-catch (class->general pd-class)
  (let ((pd-class ((change-text (list (cons "_" " ") (cons "-" " "))) pd-class)))
    (case pd-class
      (("catalysis" "stimulation") "stimulation")
      (("modulation") "modulation")
      (("inhibition") "inhibition")
      (("necessary stimulation") "necessary stimulation")
      (else
        pd-class))))

(define-catch (to-af-class pd-class)
  (let ((pd-class ((change-text (list (cons "_" " ") (cons "-" " "))) pd-class)))
    (case pd-class
      (("process" "association" "dissociation") "positive influence")
      (("catalysis" "stimulation") "positive influence")
      (("modulation") "unknown influence")
      (("inhibition") "negative influence")
      (("necessary stimulation") "necessary stimulation")
      (("simple chemical" "simple chemical multimer" "macromolecule" "macromolecule multimer" "unspecified entity" "nucleic acid feature" "complex" "complex multimer") "biological activity")
      (("perturbing agent") "perturbation")
      (("phenotype") "phenotype")
      (else
        pd-class))))
        ; (errorf "unknown PD class for translation to AF: ~a" pd-class)))))

(define (inverse-class af-class)
  (case af-class
    (("positive influence" "necessary stimulation") "negative influence")
    (("negative influence") "positive influence")
    (("unknown influence") "unknown influence")
    (else af-class)))
