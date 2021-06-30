#lang racket

(require "../server/libs/odysseus/lib/_all.rkt")
(require "../server/libs/odysseus_modules/pd2af/xml2xml.rkt")
(require "../server/libs/odysseus_modules/pd2af/xml2pd.rkt")
(require "../server/libs/odysseus_modules/pd2af/pd2pd.rkt")
(require "../server/libs/odysseus_modules/pd2af/pd2af.rkt")
(require "../server/libs/odysseus_modules/pd2af/af2af.rkt")
(require "../server/libs/odysseus_modules/pd2af/translate.rkt")
(require "../server/libs/odysseus_modules/sbgn/common.rkt")
(require "../server/libs/odysseus_modules/sbgn/context.rkt")
(require "../server/libs/odysseus_modules/sbgn/ctx2xml.rkt")

(provide (all-defined-out))

(define (test-xml xml-file)
  (let* (
        (sbgn_ml (read-file xml-file))
        (af-xml (sbgn-ml->af-xml sbgn_ml))
        )
  (write-file "../html/generated/result.sbgn" af-xml)
))

; (test-xml "../knowledge/sbgn_examples/metabolismregulation.org/F100-aspirin-V003A_2.sbgn")
(test-xml "../knowledge/sbgn_examples/bigger_maps/mastcellv40.sbgn")
