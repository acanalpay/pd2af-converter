#lang racket

(require racket/file)
(require "../lib/_all.rkt")

(provide (all-defined-out))

;(define ODYSSEUS "c:/denis/denis_core/odysseus/")
;(define ODYSSEUS-RELEASES "c:/denis/denis_core/odysseus-releases/")

(define (normalize-path astr)
  (string-replace astr "//" "/"))

(define (extract-files files-set #:new-directory (new-directory "") #:exception-set (exception-set #f) #:extract-to (extract-to ODYSSEUS-RELEASES))
  (let ((new-root (string->path (str ODYSSEUS-RELEASES new-directory))))
    ;(make-directory* new-root)
    (for ((file files-set))
      (let ((old-path (string->path (str ODYSSEUS file)))
            (new-path (string->path (normalize-path (str extract-to "/" new-directory "/" file)))))
        ;(when (directory-exists? old-path) (make-directory* new-path))
        (delete-directory/files
            new-path
            #:must-exist? #f)
        (copy-directory/files
          old-path
          new-path)))
    (when exception-set
      (for ((file exception-set))
        (let* ((new-path (normalize-path (str extract-to "/" new-directory "/" file))))
          (delete-directory/files (string->path new-path)))))))
