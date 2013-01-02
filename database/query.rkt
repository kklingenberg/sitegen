#lang racket

;; Make queries from models.

;; Examples:
;; specific cats:      (select-from cat '(and (<= age 3) (= color "brown")))
;; great cats:         (select-from cat '(like name "% the great"))
;; cats owned by Owen: (select-from cat '(related owner (= firstname "Owen")))
;; persons with cats:  (select-related 'owner (select-from cat))
;;  with old cats:     (select-related 'owner (select-from cat '(>= age 9)))
;;  without cats:      (select-related '(isnot owner) (select-from cat))

(require "../utils.rkt" "./model.rkt")

(provide select-from select-related order-by
         parse-qexpr filters
         (contract-out
          [struct qstmt ((model model?)
                         (qstring string?)
                         (params (listof any/c)))]))

; A select-from form is interpreted to a qstmt.
(struct qstmt (model qstring params))

; Extract the filters part of a qstmt (i.e. the first "where")
; filters: qstmt -> string
(define (filters query)
  (let* ([qstring (qstmt-qstring query)]
         [match (regexp-match-positions #rx" where " qstring)])
    (if match (substring qstring (caar match)) "")))

; simple predicates
(define (make-op op)
  (lambda (fieldname value model)
    (if (get-field fieldname model)
        (qstmt model (string-append (model-name model) "."
                                    fieldname op "?") `(,value))
        (raise-arguments-error 'query
                               "reference to a field that doesn't exist"
                               "field" fieldname
                               "model" (model-name model)))))

(define lt/p   (make-op " < "))
(define lte/p  (make-op " <= "))
(define eq/p   (make-op " = "))
(define gt/p   (make-op " > "))
(define gte/p  (make-op " >= "))
(define neq/p  (make-op " != "))
(define like/p (make-op " like "))

; combinators for the predicates
(define (make-joiner inter)
  (lambda (preds model)
    (qstmt model
           (string-append "(" (join inter (map qstmt-qstring preds)) ")")
           (apply append (map qstmt-params preds)))))

(define and/pc (make-joiner " and "))
(define or/pc (make-joiner " or "))

(define (not/pc pred model)
  (qstmt model
         (string-append "not (" (qstmt-qstring pred) ")")
         (qstmt-params pred)))


; follow-fk: field -> model -> model
(define (follow-fk fk-name model)
  (let ([fk (get-field fk-name model)])
    (if (field? fk)
        (cond [(foreign-key? (field-type fk))
               (foreign-key-referenced (field-type fk))]
              [(self-key? (field-type fk)) model]
              [else #f])
        #f)))


; related/p: string -> qexpr -> model -> qstmt
(define (related/p fieldname qexpr model)
  (let ([ref-model (follow-fk fieldname model)])
    (if ref-model
        (let* ([substmt   (parse-qexpr qexpr ref-model)]
               [subquery  (qstmt-qstring substmt)]
               [subparams (qstmt-params substmt)]
               [ref-pk    (get-pk ref-model)])
          (qstmt model
                 (string-append (model-name model) "."
                                fieldname " in (select "
                                (model-name ref-model) "."
                                (field-name ref-pk) " from "
                                (model-name ref-model) " where "
                                subquery ")")
                 subparams))
        (raise-arguments-error 'select-from
                               "field doesn't exist or isn't a foreign key"
                               "field" fieldname
                               "model" (model-name model)))))


(define (parse-args args)
  (list (symbol->string (car args)) (cadr args)))


; parse-qexpr: qexpr -> model -> qstmt
(define (parse-qexpr qexpr model)
  (if (null? qexpr)
      (qstmt model "" '())
      (case (car qexpr)
        [(and)  (and/pc (map (lambda (qx) (parse-qexpr qx model)) (cdr qexpr))
                        model)]
        [(or)   (or/pc (map (lambda (qx) (parse-qexpr qx model)) (cdr qexpr))
                       model)]
        [(not)  (not/pc (parse-qexpr (cadr qexpr) model) model)]
        [(<)    (apply lt/p (append (parse-args (cdr qexpr)) (list model)))]
        [(<=)   (apply lte/p (append (parse-args (cdr qexpr)) (list model)))]
        [(=)    (apply eq/p (append (parse-args (cdr qexpr)) (list model)))]
        [(>)    (apply gt/p (append (parse-args (cdr qexpr)) (list model)))]
        [(>=)   (apply gte/p (append (parse-args (cdr qexpr)) (list model)))]
        [(/=)   (apply neq/p (append (parse-args (cdr qexpr)) (list model)))]
        [(like) (apply like/p (append (parse-args (cdr qexpr)) (list model)))]
        [(related)
         (apply related/p (append (parse-args (cdr qexpr)) (list model)))]
        [else   (raise-arguments-error 'select-from
                                       "can't apply unknown filter"
                                       "filter" (car qexpr))])))


(define (encode-fields model)
  (join ", " (map (lambda (f)
                    (string-append (model-name model) "."
                                   (field-name f)))
                  (model-fields model))))


; begin-select-from: model -> qstmt
; begin-select-from: qstmt -> qstmt
(define (begin-select-from model/query)
  (cond [(model? model/query)
         (qstmt model/query (string-append "select " (encode-fields model/query)
                                           " from " (model-name model/query))
                '())]
        [(qstmt? model/query)
         (qstmt (qstmt-model model/query)
                (string-append "select "
                               (encode-fields (qstmt-model model/query))
                               " from (" (qstmt-qstring model/query) ")")
                (qstmt-params model/query))]))


; query: model -> qexpr -> qstmt
; query: qstmt -> qexpr -> qstmt
(define (select-from model/query [qexpr '()])
  (let* ([start (begin-select-from model/query)]
         [model (qstmt-model start)])
    (cond [(null? qexpr) start]
          [else (let* ([stmt (parse-qexpr qexpr model)]
                       [qstring (qstmt-qstring stmt)]
                       [params (qstmt-params stmt)])
                  (qstmt model
                         (string-append (qstmt-qstring start) " where " qstring)
                         (append (qstmt-params start) params)))])))


; parse-relexpr: relexpr -> qstmt -> qstmt
(define (parse-relexpr relexpr set)
  (define (nest fieldname modifier)
    (let* ([set-model (qstmt-model set)]
           [ref-model (follow-fk fieldname set-model)])
      (if ref-model
          (let ([pk (get-pk ref-model)])
            (qstmt ref-model (string-append (model-name ref-model) "."
                                            (field-name pk) modifier
                                            " in (select "
                                            (model-name set-model) "."
                                            fieldname " from ("
                                            (qstmt-qstring set) "))")
                   (qstmt-params set)))
          (raise-arguments-error 'select-related
                                 "relation doesn't exist"
                                 "related field" fieldname))))
  (cond [(symbol? relexpr) (nest (symbol->string relexpr) "")]
        [(and (list? relexpr) (equal? (car relexpr) 'isnot))
         (nest (symbol->string (cadr relexpr)) " not")]
        [else (raise-arguments-error 'select-related
                                     "can't apply unknown relation"
                                     "filter" relexpr)]))


; select-related: relexpr -> qstmt -> qexpr -> qstmt
; select-related: relexpr -> model -> qexpr -> qstmt
(define (select-related relexpr superset [filters '()])
  (let* ([relstmt (parse-relexpr relexpr
                                 (cond [(model? superset) (select-from superset)]
                                       [(qstmt? superset) superset]))]
         [relmodel (qstmt-model relstmt)]
         [partial-stmt (parse-qexpr filters relmodel)])
    (qstmt relmodel
           (string-append (qstmt-qstring (begin-select-from relmodel)) " where ("
                          ; relmodel is always a model, so the params are ignored
                          (qstmt-qstring relstmt)
                          (if (null? filters) ""
                              (string-append " and "
                                             (qstmt-qstring partial-stmt))) ")")
           (append (qstmt-params relstmt) (qstmt-params partial-stmt)))))


; order-by: field -> order -> qstmt -> qstmt
(define (order-by field order query)
  (let ([f (get-field (symbol->string field) (qstmt-model query))]
        [ord (if (equal? order 'asc) "asc" "desc")]
        [mname (model-name (qstmt-model query))])
    (if f (qstmt (qstmt-model query)
                 (string-append (qstmt-qstring query)
                                " order by " mname "." (field-name f)
                                " " ord)
                 (qstmt-params query))
        (raise-arguments-error 'order-by
                               "can't order by unknown field"
                               "field" field))))


(module+ test
         (define-model person
           (field "firstname" (plain-field "string")))
         (define-model cat
           (field "age" (plain-field "int"))
           (field "color" (plain-field "string"))
           (field "owner" (foreign-key person)))

         (display "TESTING database/query.rkt\n\n")

         (let ([st (select-from cat
                                '(or (not (< age 3))
                                     (and (> age 7) (= color "red"))))])
           (print (qstmt-qstring st))
           (print (qstmt-params st)))
         (display "\n--\n")

         (let ([st (select-from cat '(related owner (= firstname "Owen")))])
           (print (qstmt-qstring st))
           (print (qstmt-params st)))
         (display "\n--\n")

         (let ([st (select-from (select-from cat '(> age 3)) '(< age 7))])
           (print (qstmt-qstring st))
           (print (qstmt-params st)))
         (display "\n--\n")

         (let ([st (select-related 'owner cat)])
           (print (qstmt-qstring st))
           (print (qstmt-params st)))
         (display "\n--\n")

         (let ([st (select-related 'owner (select-from cat))])
           (print (qstmt-qstring st))
           (print (qstmt-params st)))
         (display "\n--\n")

         (let ([st (select-related '(isnot owner)
                                   (select-from cat
                                                '(and (> age 5)
                                                      (= color "white")))
                                   '(= firstname "Bob"))])
           (print (qstmt-qstring (order-by 'firstname 'asc st)))
           (print (qstmt-params st)))
         (display "\n------------\n"))
