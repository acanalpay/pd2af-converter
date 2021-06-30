#lang racket

(require "../server/libs/odysseus/lib/_all.rkt")
(require "../server/libs/odysseus/tabtree-format/tab-tree.rkt")
(require "../server/libs/odysseus/tabtree-format/utils.rkt")
(require "../server/libs/odysseus/tabtree-format/template-functions.rkt")
(require "../server/libs/odysseus/tabtree-format/html.rkt")
(require "../server/libs/odysseus/ontology/kvlist.rkt")
(require "../server/libs/odysseus_modules/pd2af/translate.rkt")
(require "../server/libs/odysseus_modules/sbgn/sexp2ctx.rkt")
(require "../server/libs/odysseus_modules/sbgn/ctx2xml.rkt")
(require "../server/libs/odysseus_modules/sbgn/sexp.rkt")
(require "../server/libs/odysseus_modules/sbgn/common.rkt")
(require "../server/libs/odysseus_modules/sbgn/test.rkt")
(require "template-functions.rkt")
(require "../tests/test-case-macros.rkt")

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(write-file "../html/index.html" (process-html-template "../templates/index.t" #:tabtree-root "../knowledge" #:namespace ns))
(write-file "../html/specification.html" (process-html-template "../templates/specification.t" #:tabtree-root "../tests" #:namespace ns))
(write-file "../html/cases.html" (process-html-template "../templates/cases.t" #:tabtree-root "../tests" #:namespace ns))
(write-file "../html/translator.html" (process-html-template "../templates/translator.t" #:tabtree-root "../knowledge" #:namespace ns))
