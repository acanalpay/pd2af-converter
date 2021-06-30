#lang racket

(require compatibility/defmacro (for-syntax racket/list racket/format))

(require "../server/libs/odysseus/lib/_all.rkt")
(require "../server/libs/odysseus/graphics/svg.rkt")
(require "../server/libs/odysseus/tabtree-format/tab-tree.rkt")
(require "../server/libs/odysseus/tabtree-format/html.rkt")
(require "../server/libs/odysseus_modules/sbgn/sexp2ctx.rkt")
; (require "../../../../sbgn-pd2af/server/sbgn/translate.rkt")

(provide (all-defined-out))

(define ns (module->namespace (string->path "gen-html.rkt")))

; (define newt-prefix-url "http://web.newteditor.org/?URL=https://denis-shirshov.ru/for/alexander-mazein/rules")
(define newt-prefix-url "http://web.newteditor.org/?URL=http://pd2af.rusvegia.com/rules-auto")

(define (yandex-counter)
  (read-file "../templates/parts/yandex-counter.part"))

(define (top (curpage "index"))
  (string-replace (read-file "../templates/parts/top.part") "<curpage_id>" curpage))

(define (head)
  (read-file "../templates/parts/head.part"))

(define (footer)
  (read-file "../templates/parts/footer.part"))

(define-macro (expandable . args)
  (let ((subst-table (for/fold
                        ((res empty))
                        ((arg args))
                        (append
                          (list (cons (format "<<~a>>" (first arg)) (~a (second arg))))
                          res))))
  `((change-text ',subst-table)
      (read-file "../templates/parts/expandable.part"))))

(define-macro (expandable-cases . args)
  (let ((subst-table (for/fold
                        ((res empty))
                        ((arg args))
                        (append
                          (list (cons (format "<<~a>>" (first arg)) (~a (second arg))))
                          res))))
  `((change-text ',subst-table)
      (read-file "../templates/parts/expandable_cases.part"))))

(define (exists? var)
  (and var (non-empty-string? var)))

(define (preprocess-lisp-sbgn astr)
  (let* ((symbols-to-change
          (list
            (cons "`" "\"")
            (cons "\"" "\\\"")
            (cons "," "&")))
        (res (for/fold
                ((res astr))
                ((s symbols-to-change))
                (string-replace res (car s) (cdr s)))))
    (read (open-input-string res))))

(define-catch (get-pd-str basepath item)
  (let* (
        (folder ($ folder item))
        (folder-path (str basepath "/" folder))
        (pd-coors ($ pd-coors item))
        (pd-str ($ pd item))
        (compartments ($ compartments item))
        (pd-full ($ pd-full item))
        (pd-file ($ pd-file item))
        (diagram-files-in-folder ($ diagram-files-in-folder item))
        (pd-full-str (or
                        (and pd-full (format "(pd ~a)" pd-full))
                        (and pd-file (read-file pd-file))
                        (and diagram-files-in-folder (read-file (str folder-path "/diagram.pd")))
                        (if compartments
                          (format "(pd ~a ~a)" pd-coors pd-str)
                          (format "(pd (default:compartment ~a ~a))" pd-coors pd-str)))))
      pd-full-str))

(define-catch (get-af-str basepath item)
  (let* (
        (folder ($ folder item))
        (folder-path (str basepath "/" folder))
        (af-coors ($ af-coors item))
        (af-str ($ af item))
        (compartments ($ compartments item))
        (af-full ($ af-full item))
        (af-file ($ af-file item))
        (diagram-files-in-folder ($ diagram-files-in-folder item))
        (af-full-str (or
                        (and af-full (format "(af ~a)" af-full))
                        (and af-file (read-file af-file))
                        (and diagram-files-in-folder (read-file (str folder-path "/diagram.af")))
                        (if compartments
                          (format "(af ~a)" af-str)
                          (format "(af (default:compartment ~a ~a))" af-coors af-str)))))
      af-full-str))

(define (calculate-img-width img-path)
  (if (file-exists? img-path)
    (let* ((image-geometry (get-image-geometry img-path))
          (w0 ($ width image-geometry))
          (w (*r 0.8 w0)))
      (~a w))
    (~a 0)))

(define-catch (go-through-tests test-file)
    (load-data-from-file test-file ns))
