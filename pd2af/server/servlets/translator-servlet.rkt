#lang racket
(require web-server/servlet web-server/servlet-env web-server/formlets)
(require "../libs/odysseus/lib/_all.rkt")
(require "../libs/odysseus/deploy/system.rkt")
(require "../settings.rkt")
(require "../libs/odysseus_modules/pd2af/xml2xml.rkt")
(require "../libs/odysseus_modules/pd2af/xml2pd.rkt")
(require "../libs/odysseus_modules/pd2af/pd2pd.rkt")
(require "../libs/odysseus_modules/pd2af/pd2af.rkt")
(require "../libs/odysseus_modules/pd2af/af2af.rkt")
(require "../libs/odysseus_modules/pd2af/translate.rkt")
(require "../libs/odysseus_modules/sbgn/common.rkt")
(require "../libs/odysseus_modules/sbgn/context.rkt")
(require "../libs/odysseus_modules/sbgn/ctx2xml.rkt")

(provide (all-defined-out))

(define (make-json-string val)
  (string->bytes/utf-8
    (format "~a"
      (cond
        ((not val) "Invalid format")
        (else (alist->json val))))))

; sudo -b nohup racket /server/pd2af/code/start-server.rkt

(define (translator req)
  (set-id #:reset #t) ; reset counter, otherwise it will accumulate values up to thousands and godzillions
  (with-handlers
    ((exn:fail? (λ (err)
                    (response/full
                      202
                      #"Sends data back"
                      (current-seconds)
                      #"application/json; charset=utf-8"
                      (list)
                      (list
                        (make-json-string `((error_message ,(clean-newlines (exn-message err)))))
                      )))))
      (let* (
            (req (request-bindings req))
            (sbgn_ml (cdr (assoc 'file req)))
            (filename (cdr (assoc 'filename req)))
            (filename (first (split filename ".")))
            (sbgn_ml (bytes->string/utf-8 sbgn_ml))

            (af-xml (sbgn-ml->af-xml sbgn_ml))

            (generated-filename (format "~a.af.sbgn" filename))
            (file-url (str (get-settings 'root-url) "/generated/" generated-filename))
            (file-path (get-settings 'paths 'lcsb))
            (_ (write-file (format "~a/~a" file-path generated-filename) af-xml))
            (resp (make-json-string `((af_filename ,generated-filename) (af_fileurl ,file-url)))))
      	(response/full
          200
          #"Sends data back"
          (current-seconds)
          #"application/json; charset=utf-8"
          (list)
          (list
            resp
          )))))
