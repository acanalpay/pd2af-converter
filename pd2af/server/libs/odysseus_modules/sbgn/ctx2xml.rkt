#lang racket

(require compatibility/defmacro)
(require racket/syntax)
(require "../../odysseus/lib/_all.rkt")
(require "types.rkt")
(require "common.rkt")
(require "geometry.rkt")
(require "context.rkt")
(require "sexp.rkt")
(require sxml)

(provide (all-defined-out))

;; helpers
(define-catch (get-cardinality cardinality-string (source-or-target #f))
  (let* (
        ; original ',' cannot be read by READ function, hence we substitute in sbgn-lisp ',' for '&' somewhere before parsing
        (cardinality (if cardinality-string (split cardinality-string "&&") #f))
        (sources-cardinality (if cardinality (first cardinality) #f))
        (sources-cardinality (if sources-cardinality (split sources-cardinality "&") #f))
        (targets-cardinality (if (or (not cardinality) (empty? (cdr cardinality))) #f (second cardinality)))
        (targets-cardinality (if targets-cardinality (split targets-cardinality "&") #f)))
    (case source-or-target
      ((sources) sources-cardinality)
      ((targets) targets-cardinality)
      (else (append sources-cardinality targets-cardinality)))))

(define (NotDefaultCompartment? element)
  (let ((class ($ class element)))
    (and
      (equal? class "compartment")
      (not-equal? ($ id element) default-compartment-name))))

;; calculate coordinates
(define-catch (calculate-element-geometry el)
  (let* (
        (w (cond
            ((or* ActivityNode? NamedEPN? el) ($ el-w defaults-1))
            ((SourceSink? el) ($ sink-w defaults-1))
            (else ($ el-w defaults-1))))
        (h (cond
            ((or* ActivityNode? NamedEPN? el) ($ el-h defaults-1))
            ((SourceSink? el) ($ sink-h defaults-1))
            (else ($ el-h defaults-1))))
        (coors (get-coors-from-q el w h))
        (x (or ($ x coors) (get-random-coor W)))
        (y (or ($ y coors) (get-random-coor H)))
        )
    (hash 'x x 'y y 'w w 'h h)))

(define-catch (calculate-port-element-geometry hyperarc context)
  (let* (
        (el hyperarc)
        (id ($ id el))
        (type ($ type hyperarc))
        (source-ids ($ sources el))
        (sources (map (λ (source-id) (&& source-id context)) source-ids))
        (target-ids ($ targets el))
        (targets (map (λ (target-id) (&& target-id context)) target-ids))
        ; (_ (--- el (append sources targets)))
        (a-mass-center (mass-center (append sources targets)))
        (xc ($ x a-mass-center))
        (yc ($ y a-mass-center))
        (w (if (LogicalOperator? hyperarc) ($ gate-w defaults-1) ($ process-w defaults-1)))
        (h (if (LogicalOperator? hyperarc) ($ gate-h defaults-1) ($ process-h defaults-1)))
        (x (floor (- xc (/ w 2.0))))
        (y (floor (- yc (/ h 2.0))))
        (port-coors (calculate-port-coors sources x y ($ gate-margin defaults-1) #:w w #:h h))
        )
    (hash-union el
      (hash 'x x 'y y 'w w 'h h
            'in-x ($ port-in-x port-coors) 'in-y ($ port-in-y port-coors)
            'out-x ($ port-out-x port-coors) 'out-y ($ port-out-y port-coors)))))

(define-catch (calculate-complex-components-xy complex)
  (let* (
        (component-ids ($ components complex))
        (complex-x ($ x complex))
        (complex-y ($ y complex))
        (n (length component-ids))
        (el-w ($ el-w defaults-1))
        (el-h ($ el-h defaults-1))
        ; (complex-w (case n
        ;               ((1 2 3) (+ el-w 20))
        ;               (else (* 2 (+ el-w 20)))))
        (complex-w (+ el-w 20))
        ; (complex-h (case n
        ;               ((1 2 3) (* n (+ el-h 10)))
        ;               (else (* (/c n 2) (+ el-h 10)))))
        (complex-h (* n (+ el-h 10)))
        )
        (let loop ((res-hash (hash)) (ids component-ids))
          (if (empty? ids)
            res-hash
            (let* ((count (- n (length ids)))
                  ; (column (if (odd? count) 1 0))
                  (column 0)
                  ; (row (/f count 2))
                  (row count)
                  (x (+ (+ complex-x 10) (* el-w column)))
                  (y (+ (+ complex-y 5) (* (+ el-h ($ component-vertical-gap defaults-1)) row))))
              (loop
                (hash-union res-hash (hash (car ids) (hash 'x x 'y y 'w el-w 'h el-h)))
                (cdr ids)))))))

(define (get-random-coor limit)
  (random limit))

(define-catch (get-coors-from-q el (x-w 0) (x-h 0))
  (let* (
        (q ($ q el))
        (qx (->number ($ qx el)))
        (qy (->number ($ qy el)))
        (q-w ($ square-w defaults-1))
        (q-h ($ square-h defaults-1)))
    (cond
      (q
        (let* ((qs (split q "-"))
              (qx (or (->number (first qs)) 0))
              (qy (or (->number (second qs)) 0)))
          (centrify x-w x-h (* qx (* 0.5 q-w)) (* qy q-h) q-w q-h)))
      ((and qx qy)
        (centrify x-w x-h (* qx (* 0.5 q-w)) (* qy q-h) q-w q-h))
      (else
        (hash 'x ($ default-x defaults-1) 'y ($ default-y defaults-1))))))

(define-catch (get-compartment-coors compartment-id context)
    (let ((elements-of-compartment (filter (λ (x) (equal? ($ compartment x) compartment-id)) context)))
      (cond
        ((empty? elements-of-compartment)
          (hash 'x 0 'y 0 'w (* 2 ($ compartment-margin-x defaults-1)) 'h (* 2 ($ compartment-margin-y defaults-1))))
        (else
          (let*-values
              (((x-min y-min x-max y-max)
                (for/fold
                  ((x-min #f) (y-min #f) (x-max #f) (y-max #f))
                  ((el elements-of-compartment))
                  (let (
                        (el-x (or (->number ($ x el)) 0))
                        (el-y (or (->number ($ y el)) 0))
                        (el-w (or (->number ($ w el)) ($ compartment-w defaults)))
                        (el-h (or (->number ($ h el)) ($ compartment-w defaults))))
                    (values
                      (if (or (not x-min) (< el-x x-min)) el-x x-min)
                      (if (or (not y-min) (< el-y y-min)) el-y y-min)
                      (if (or (not x-max) (> (+ el-x el-w) x-max)) (+ el-x el-w) x-max)
                      (if (or (not y-max) (> (+ el-y el-h) y-max)) (+ el-y el-h) y-max)))))
              ((x y w h) (values
                        (- x-min ($ compartment-margin-x defaults-1))
                        (- y-min ($ compartment-margin-y defaults-1))
                        (+ (- x-max x-min) (* 2 ($ compartment-margin-x defaults-1)))
                        (+ (- y-max y-min) (* 2 ($ compartment-margin-y defaults-1)))))
              ((x y) (values
                        (if (< x 0) 0 x)
                        (if (< y 0) 0 y))))
            (hash 'x x 'y y 'w w 'h h))))))

(define-catch (expand-context-xy context)
  ; (---- context)
  ; (--- "")
  (let* (
        (context
            ;; calculate geometry for epn nodes
            (for/fold
              ((res empty))
              ((el context))
              (cond
                ((or* EPN? ActivityNode? el)
                  (let ((coors (calculate-element-geometry el)))
                    (pushr res (hash-union el (hash 'x ($ x coors) 'y ($ y coors) 'w ($ w coors) 'h ($ h coors))))))
                (else (pushr res el)))))
        (context
            ;; calculate xy for subelements in the complexes
            (map
              (λ (el)
                (cond
                  ((Complex? el) (hash-union
                                    (hash 'components (calculate-complex-components-xy el))
                                    el))
                  (else el)))
              context))
        (context
            ;; calculate sizes of complexes
            (for/fold
              ((res empty))
              ((el context))
              (cond
                ((Complex? el)
                  (let* (
                        (components (hash-values ($ components el))))
                    (let-values (((xmin ymin xmax ymax)
                                    (for/fold
                                      ((xmin ($ x el)) (ymin ($ y el)) (xmax (+ ($ x el) ($ w el))) (ymax (+ ($ y el) ($ h el))))
                                      ((component components))
                                      (let* ((x ($ x component))
                                            (y ($ y component))
                                            (w ($ w component))
                                            (h ($ h component)))
                                        (values
                                          (if (< x xmin) x xmin)
                                          (if (< y ymin) y ymin)
                                          (if (> (+ x w) xmax) (+ x w) xmax)
                                          (if (> (+ y h) ymax) (+ y h) ymax))))))
                    (pushr res (hash-union (hash 'x xmin 'y ymin 'w (+ 10 (- xmax xmin)) 'h (+ 10 (- ymax ymin))) el)))))
                (else (pushr res el)))))
        (context
          ;; calculate location for process nodes
            (for/fold
              ((res empty))
              ((el context))
              (cond
                ((ProcessNode? el) (pushr res (calculate-port-element-geometry el context)))
                (else (pushr res el)))))
        (context
          ;; calculate location for logical nodes, right after we know coordinates of process nodes
            (for/fold
              ((res empty))
              ((el context))
              (cond
                ((LogicalOperator? el) (pushr res (calculate-port-element-geometry el context)))
                (else (pushr res el)))))
        (context
            ;; calculate start and end points for arcs
            (for/fold
              ((res empty))
              ((el context))
              (cond
                ((or* FluxArc? LogicArc? el)
                  (let* (
                        (source (&& (get-source-id el) context '(id in-id out-id)))
                        (target (&& (get-target-id el) context '(id in-id out-id)))
                        (coors (calculate-arc-coors ($ x source) ($ y source) ($ w source) ($ h source) ($ x target) ($ y target) ($ w target) ($ h target) (list ($ id source) ($ id target))))
                        (x1 (if ($ out-id source) ($ out-x source) ($ x1 coors)))
                        (y1 (if ($ out-id source) ($ out-y source) ($ y1 coors)))
                        (x2 (if ($ in-id target) ($ in-x target) ($ x2 coors)))
                        (y2 (if ($ in-id target) ($ in-y target) ($ y2 coors)))
                        )
                  (pushr res (hash-union (hash 'x1 x1 'y1 y1 'x2 x2 'y2 y2) el))))
                ((or* ModulationArc? ActivityArc? el)
                  (let* (
                        (source (&& (get-source-id el) context '(id in-id out-id)))
                        (target (&& (get-target-id el) context '(id)))
                        (coors (calculate-arc-coors ($ x source) ($ y source) ($ w source) ($ h source) ($ x target) ($ y target) ($ w target) ($ h target) (list ($ id source) ($ id target))))
                        (x1 ($ x1 coors))
                        (y1 ($ y1 coors))
                        (x2 ($ x2 coors))
                        (y2 ($ y2 coors))
                        )
                  (pushr res (hash-union (hash 'x1 x1 'y1 y1 'x2 x2 'y2 y2) el))))
                (else (pushr res el)))))
        (context
            ;; calculate sizes of compartments
            (for/fold
              ((res empty))
              ((el context))
              (cond
                ((Container? el)
                  (let* (
                        (coors (get-compartment-coors ($ id el) context))
                        )
                    (pushr res (hash-union el (hash 'x ($ x coors) 'y ($ y coors) 'w ($ w coors) 'h ($ h coors))))))
                (else (pushr res el)))))
        )
    context))

;; context -> context
(define (new-id type)
  (set-id #:prefix (str (string-replace type " " "-") "-")))

(define-catch (make-arc #:source-id source-id #:target-id target-id #:type (type "logic arc") #:cardinality (cardinality 1))
  (hash-union
    (if (and cardinality (> cardinality 1))
      (hash 'cardinality cardinality)
      (hash))
    (hash 'class type 'id (new-id type) 'source source-id 'target target-id )))

(define-catch (expand-hyperarcs context)
  (for/fold
    ((res empty))
    ((el context))
    (cond
      ((not (Hyperarc? el)) (pushr-unique res el)) ; skip expanding, if non-hyperarc elements
      (else (append-unique res (expand-hyperarc el context))))))

(define-catch (expand-logical-combination hyperarc combination context)
  (let* (
        (logical-type (->string (car combination)))
        (id (new-id logical-type))
        (in-id (format-symbol "~a-in" id))
        (out-id (format-symbol "~a-out" id))
        (inner-items (cdr combination)) ; sources for logical gate itself
        (inner-node-ids (filter (λ (item) (node-id? item context)) inner-items))
        (inner-logical-combinations (filter logical-combination? inner-items))
        (inner-logical-combinations-expansion (map
                                                (λ (combination) (expand-logical-combination hyperarc combination context))
                                                inner-logical-combinations))
        ; ids for logical nodes that are direct neighbours for current hyperarc target:
        (inner-logical-gates-ids (map
                                  (λ (els) (and (car els) ($ id (car els))))
                                  inner-logical-combinations-expansion))
        (gate-source-ids (append inner-node-ids inner-logical-gates-ids))
        ; (_ (--- ">>" inner-items inner-node-ids gate-source-ids))
        (gate (hash 'class logical-type 'id id 'in-id in-id 'out-id out-id 'sources gate-source-ids 'targets ($ targets hyperarc)))
        (logic-arcs (for/fold
                      ((cur-res empty))
                      ((gate-source-id gate-source-ids))
                      (pushr cur-res (make-arc #:source-id gate-source-id #:target-id in-id #:type "logic arc")))))
    (append (list gate) logic-arcs (flatten inner-logical-combinations-expansion))))

(define-catch (expand-hyperarc el context)
  (let* ((id ($ id el))
        (type ($ class el))
        (source-ids ($ sources el))
        (target-ids ($ targets el))
        (in-id (format-symbol "~a-in" id))
        (out-id (format-symbol "~a-out" id)))
    (cond
      ((ReversibleProcess? el)
        (let* ((process-node (hash-union el (hash 'in-id in-id 'out-id out-id)))
              (consumption-arcs
                (for/fold
                  ((cur-res empty))
                  ((source-id source-ids))
                  (pushr cur-res (make-arc #:source-id in-id  #:target-id source-id #:type "production"))))
              (production-arcs
                (for/fold
                  ((cur-res empty))
                  ((target-id target-ids))
                  (pushr cur-res (make-arc #:source-id out-id #:target-id target-id #:type "production")))))
            (append (list process-node) consumption-arcs production-arcs)))
      ((GeneralProcess? el)
        (let* ((process-node (hash-union el (hash 'in-id in-id 'out-id out-id)))
              (consumption-arcs
                (for/fold
                  ((cur-res empty))
                  ((source-id source-ids))
                  (pushr cur-res (make-arc #:source-id source-id  #:target-id in-id #:type "consumption"))))
              (production-arcs
                (for/fold
                  ((cur-res empty))
                  ((target-id target-ids))
                  (pushr cur-res (make-arc #:source-id out-id  #:target-id target-id #:type "production")))))
            (append (list process-node) consumption-arcs production-arcs)))
      ((or* ModulationArc? ActivityArc? el)
            (cond
              ((and (one-element? source-ids) (one-element? target-ids))
                (list (make-arc #:source-id (car source-ids) #:target-id (car target-ids) #:type type)))
              ((and (one-element? source-ids) (several-elements? target-ids))
                (let* ((and-gate (hash 'class "and" 'id id 'in-id in-id 'out-id out-id 'sources source-ids 'targets target-ids))
                      (logic-arc (make-arc #:source-id (car source-ids) #:target-id in-id #:type "logic arc"))
                      (modulation-arcs
                                 (for/fold
                                    ((cur-res empty))
                                    ((target-id target-ids))
                                    (pushr cur-res (make-arc #:source-id out-id #:target-id target-id #:type type)))))
                  (append modulation-arcs (list and-gate logic-arc))))
              ((and
                (several-elements? source-ids)
                (not (LogicalPrefix? (car source-ids))))
                  (expand-hyperarc (hash-union (hash 'sources `(and ,@source-ids)) el) context))
              ((logical-combination? source-ids)
                (let* ((expanded-logical-combination (expand-logical-combination el source-ids context))
                      (gate-out-id ($ out-id (first expanded-logical-combination)))
                      (modulation-arcs
                        (for/fold
                          ((cur-res empty))
                          ((target-id target-ids))
                          (pushr cur-res (make-arc #:source-id gate-out-id #:target-id target-id #:type type)))))
                  (append modulation-arcs expanded-logical-combination)))
              (else empty)))
      (else (errorf "given argument is not Hyperarc: ~a" el)))))

;; create sbgn elements in sxml
(define-catch (build-complex element context)
  (let* (
        (complex-id (->string ($ id element)))
        (compartment ($ compartment element))
        (compartment (if (equal? compartment default-compartment-name) #f compartment))
        (compartment (if compartment `(compartmentRef ,(->string compartment)) '$f))
        (components ($ components element))
        (name ($ name element))
        (label (if name
                      `(label (@ (text ,(purify-string (->string name)))))
                      '$f))
        (bbox `(bbox (@ (x ,($ x element)) (y ,($ y element)) (w ,($ w element)) (h ,($ h element)))))
        (internal-elements (filter (λ (x) (indexof? (hash-keys components) ($ id x))) context))
        (internal-elements (map (λ (x) (hash-substitute
                                          (hash-delete x 'compartment) ; remove compartment info from the element as it is inside a complex
                                          (list
                                            (cons 'id (format-symbol "~a-~a" ($ id x) complex-id)) ; mofify id of the element, to make it unique (in the case element is defined by def for use in the several complexes)
                                            (cons 'old-id ($ id x)))))
                                internal-elements))
        (context (append-unique context internal-elements))
        (content
          (for/fold
            ((sxml empty))
            ((c internal-elements))
            (let* (
                  (component-id ($ id c))
                  (coors (hash-ref components ($ old-id c)))
                  (c (hash-union coors c))
                  (subglyph (cond
                              ((Complex? c) (build-complex c context))
                              ((Node? c) (build-node c))
                              (else '$f)))
                  )
              (pushr sxml subglyph)))))
    (format-list
      `(glyph (@ (class "complex") (id ~a) ~a)

        ~a ; label
        ~a ; bbox
        ~@a) ; content
      complex-id
      compartment

      label
      bbox
      content)))

(define-catch (build-node element #:node-with-ports? (node-with-ports? #f))
  (define (build-port (id #f) (x 0) (y 0))
    `(port (@ (id ,(or id (set-id))) (x ,x) (y ,y))))
  (let* (
        (class ($ class element))
        (class (cond
                  ((ReversibleProcess? element) "process")
                  (else class)))
        (id ($ id element))
        (compartment ($ compartment element))
        (complex ($ complex element))
        (compartment (if (and compartment (not complex) (not (equal? compartment default-compartment-name))) `(compartmentRef ,(->string compartment)) '$f))
        (name (get-node-name element))
        (label (if (or* NamedEPN? Container? ActivityNode? element) `(label (@ (text ,(namefy name)))) '$f))
        (el-w ($ el-w defaults-1))
        (el-h ($ el-h defaults-1))
        (x (or ($ x element) 0))
        (y (or ($ y element) 0))
        (w (or ($ w element) el-w))
        (h (or ($ h element) el-h))
        (bbox (if (or* Node? Container? ActivityNode? element) `(bbox (@ (x ,x) (y ,y) (w ,w) (h ,h))) '$f))
        (in-port (if node-with-ports?
                    (build-port (->string ($ in-id element)) ($ in-x element) ($ in-y element))
                    '$f))
        (out-port (if node-with-ports?
                    (build-port (->string ($ out-id element)) ($ out-x element) ($ out-y element))
                    '$f))
        (uoi ($ uoi element))
        (uoi (and uoi (purify-string ($ uoi element)) uoi))
        (uoi-w ($ uoi-w defaults-1)) (uoi-h ($ uoi-h defaults-1))
        (uoi-x (+ x (/ (- w uoi-w) 2.0)))
        (uoi-y (- y (/ uoi-h 2.0)))
        (entity ($ type element))
        (entity (and entity (string-replace (str entity) "-" " ")))
        (entity-glyph (if entity
                          `(entity (@ (name ,entity)))
                          '$f))
        (entity (if (equal? entity "macromolecule") #f entity))
        (uoi-glyph (cond
                      ((and (or* NamedEPN? ActivityNode? element) (or uoi entity))
                          (format-list
                            `(glyph (@ (id ,(->string (set-id #:prefix (str id "-uoi-")))) (class "unit of information"))
                              (label (@ (text ,(if uoi uoi ""))))
                              ~a
                              (bbox (@ (x ,uoi-x) (y ,uoi-y) (w ,uoi-w) (h ,uoi-h))))
                            entity-glyph))
                      (else '$f)))
        (state-variables (minus (hash-keys element) reserved-keys))
        (state-glyphs (if (and (NamedEPN? element) (not (empty? state-variables)))
                        (for/list
                          ((state-variable state-variables) (i (in-naturals)))
                          (let* (
                                (sv-x (+ x (list-ref ($ state-variable-positions defaults-1) i)))
                                (hh (if (equal? ($ state-position defaults) 'top) 0 h)) ; state on top or on bottom
                                (sv-y (- (+ y hh) (/ ($ state-variable-d defaults-1) 2.0)))
                                (default-variable? (indexof? ($ state-variable-names defaults-1) state-variable))
                                (sv-w (if default-variable?
                                          ($ state-variable-d defaults-1)
                                          ($ state-variable-w defaults-1)))
                                (sv-h (if default-variable?
                                          ($ state-variable-d defaults-1)
                                          ($ state-variable-h defaults-1)))
                                (state-value (purify-string (->string (hash-ref element state-variable))))
                                (state-value (if (indexof? ($ empty-values defaults) state-value) "" state-value))
                                (state (if default-variable?
                                          `(state (@ (value ,state-value) (variable "")))
                                          `(state (@ (value ,state-value) (variable ,(->string state-variable)))))))
                          `(glyph (@ (id ,(->string (set-id #:prefix (str id "-state-variable-")))) (class "state variable"))
                              ,state
                              (bbox (@ (x ,sv-x) (y ,sv-y) (w ,sv-w) (h ,sv-h))))))
                        '$f))
        ; (uoi-glyph (if (current-map-is-af?) '$f uoi-glyph))
        (state-glyphs (if (and (current-map-is-af?) (list? state-glyphs))
                            (map (λ (x) '$f) state-glyphs)
                            state-glyphs))
        )
    (format-list
      `(glyph (@ (id ~s) (class ~s)
          ~a ; compartmentRef
        )
        ~a ; label
        ~a ; bbox
        ~a ; in-port
        ~a ; out-port
        ~a ; uoi-glyph
        ~@a ; state-glyphs
        )
      id
      class
      compartment

      label
      bbox
      in-port
      out-port
      uoi-glyph
      state-glyphs)))

(define-catch (build-arc element)
  (let* (
        (class ($ class element))
        (id (->string ($ id element)))
        (source-id (->string (get-source-id element)))
        (target-id (->string (get-target-id element)))
        (cardinality ($ cardinality element))
        (cardinality-glyph (if (and cardinality (not-equal? (->string cardinality) "1"))
                                    `(glyph (@ (id ,(->string (set-id #:prefix (str id "-cardinality-")))) (class "cardinality"))
                                        (label (@ (text ,cardinality)))
                                        (bbox (@ (x "0") (y "0") (w "0") (h "0"))))
                                    '$f))
        (start `(start (@ (x ,($ x1 element)) (y ,($ y1 element)))))
        (end `(end (@ (x ,($ x2 element)) (y ,($ y2 element)))))
        )
    (format-list
      `(arc (@ (class ~a) (id ~a) (source ~a) (target ~a))
        ~a ; cardinality
        ~a ; start
        ~a ; end
        )
        class
        id
        source-id
        target-id

        cardinality-glyph

        start
        end)))

(define-catch (context->sxml context sbgn-ml-version map-type)
  (let* (
        (xmlns (format "http://sbgn.org/libsbgn/~a" sbgn-ml-version))
        (W ($ W defaults-1))
        (H ($ H defaults-1))
        (el-w ($ el-w defaults-1))
        (el-h ($ el-h defaults-1))
        (process-w ($ process-w defaults-1))
        (process-h ($ process-h defaults-1))
        (sxml-wrap `(sbgn (@ (xmlns ,xmlns)) (map (@ (language ,map-type)) ~@a)))
        (context (expand-hyperarcs context))
        (context (expand-context-xy context))
        (epns (clean Env? (clean Def? context)))
        (first-level-elements (clean InternalEPN? epns)) ; Possible bug: What to do with internal elements of complex, if they are defined 'on the place'? Defined at def section?
        (body
          (for/fold
            ((sxml empty))
            ((element first-level-elements) (i (in-naturals)))
            (cond
              ((Complex? element)
                  (pushr sxml (build-complex element context)))
              ((or* LogicalOperator? ProcessNode? element)
                  (pushr sxml (build-node element #:node-with-ports? #t)))
              ((or* Arc? ActivityArc? element)
                  (pushr sxml (build-arc element)))
              ((or* Node? ActivityNode? (λ (x) (and* Container? NotDefaultCompartment? x)) element)
                  (pushr sxml (build-node element)))
              (else sxml)))))
    (format-list
      sxml-wrap
  		(order-by-tag '(glyph arc) body))))

(define-catch (context->xml context #:sbgn-ml-version (sbgn-ml-version "0.2") #:map-type (map-type "process description"))
  (current-map-type map-type)
  (let ((sxml (context->sxml context sbgn-ml-version map-type)))
      (format "~a~n~a"
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
        (srl:sxml->xml sxml))))
