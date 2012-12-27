#lang racket

;; Make queries from models.

;; Examples:
;; specific cats:      (query cat '(and (<= age 3) (= color "brown")))
;; great cats:         (query cat '(like name "% the great"))
;; cats owned by Owen: (query cat '(related owner (= firstname "Owen")))
;; persons with cats:  (query-related 'owner (query cat))
;; with old cats:      (query-related 'owner (query cat '(>= age 9)))
;; without cats:       (query-related '(isnot owner) (query cat))

(require "../utils.rkt" "./model.rkt")

(provide query query-related
         (contract-out
          [struct qstmt ((model model?)
                         (qstring string?)
                         (params (listof any/c)))]))

; A query is compiled to a qstmt.
(struct qstmt (model qstring params))

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

; nested queries

; related/p: string -> qexpr -> model -> qstmt
(define (related/p fieldname qexpr model)
  (let ([fk (get-field fieldname model)])
    (if (and fk (foreign-key? (field-type fk)))
        (let* ([referenced-model (foreign-key-referenced (field-type fk))]
               [substmt (parse-qexpr qexpr referenced-model)]
               [subquery (qstmt-qstring substmt)]
               [subparams (qstmt-params substmt)]
               [referenced-pk (get-pk referenced-model)])
          (qstmt model
                 (string-append (model-name model) "."
                                fieldname " in (select "
                                (model-name referenced-model) "."
                                (field-name referenced-pk) " from "
                                (model-name referenced-model) " where "
                                subquery ")")
                 subparams))
        (raise-arguments-error 'query
                               "field doesn't exist or isn't a foreign key"
                               "field" fieldname
                               "model" (model-name model)))))


(define (parse-args args)
  (list (symbol->string (car args)) (cadr args)))

; parse-qexpr: qexpr -> model -> qstmt
(define (parse-qexpr qexpr model)
  (if (null? qexpr)
      (list model "")
      (case (car qexpr)
        [(and) (and/pc (map (lambda (qx) (parse-qexpr qx model)) (cdr qexpr))
                        model)]
        [(or)  (or/pc (map (lambda (qx) (parse-qexpr qx model)) (cdr qexpr))
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
        [else   (raise-arguments-error 'query
                                       "can't apply unknown filter"
                                       "filter" (car qexpr))])))

(define (encode-fields model)
  (join ", " (map (lambda (f)
                    (string-append (model-name model) "."
                                   (field-name f)))
                  (model-fields model))))

; query: model -> qexpr -> qstmt
(define (query model [qexpr '()])
  (let ([start (string-append "select "
                              (encode-fields model)
                              " from " (model-name model))])
    (cond [(null? qexpr) (list model start)]
          [else (let* ([stmt (parse-qexpr qexpr model)]
                       [qstring (qstmt-qstring stmt)]
                       [params (qstmt-params stmt)])
                  (qstmt model
                         (string-append start " where " qstring)
                         params))])))

; TODO query-related
; query-related: relexpr -> qstmt -> qexpr -> qstmt
(define (query-related) '())


(module+ test
         (define person (model "person" #t
                               (list (field "firstname"
                                            (plain-field "string")))))
         (define cat (model "cat" #t
                            (list (field "age" (plain-field "int"))
                                  (field "color" (plain-field "string"))
                                  (field "owner" (foreign-key person)))))
         (display "TESTING core/query.rkt\n\n")
         (let ([st (query cat '(or (not (< age 3))
                                   (and (> age 7) (= color "red"))))])
           (print (qstmt-qstring st))
           (print (qstmt-params st)))
         (display "\n--\n")
         (let ([st (query cat '(related owner (= firstname "Owen")))])
           (print (qstmt-qstring st))
           (print (qstmt-params st)))
         (display "\n------------\n"))
