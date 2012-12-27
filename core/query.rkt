#lang racket

;; Make queries from models. Example:
;; (query cat '(and (lt age 3) (eq color "red")))

(require "../utils.rkt" "model.rkt")

(provide query)

; A query is compiled to a list with head the model being queried; the
; second element is the compiled query string and all the rest are the
; parameters, in order.

; easy predicates
(define (make-op op)
  (lambda (fieldname value model)
    (if (get-field fieldname model)
        (list model (string-append (model-name model) "."
                                   fieldname op "?") value)
        (raise-arguments-error 'query
                               "reference to a field that doesn't exist"
                               "field" fieldname))))

(define lt/p (make-op " < "))
(define lte/p (make-op " <= "))
(define eq/p (make-op " = "))
(define gt/p (make-op " > "))
(define gte/p (make-op " >= "))
(define neq/p (make-op " != "))

; combinators for the predicates
(define (make-joiner inter)
  (lambda (preds model)
    (flatten (list model
                   (string-append "(" (join inter (map cadr preds)) ")")
                   (map cddr preds)))))

(define and/pc (make-joiner " and "))
(define or/pc (make-joiner " or "))

(define (not/pc pred model)
  (flatten (list model
                 (string-append "not (" (cadr pred) ")")
                 (cddr pred))))

; TODO nested queries

(define (parse-args args)
  (list (symbol->string (car args)) (cadr args)))

; parse-qexpr: qexpr -> model -> querystmt
(define (parse-qexpr qexpr model)
  (if (null? qexpr)
      (list model "")
      (case (car qexpr)
        [(and) (and/pc (map (lambda (qx) (parse-qexpr qx model)) (cdr qexpr))
                        model)]
        [(or)  (or/pc (map (lambda (qx) (parse-qexpr qx model)) (cdr qexpr))
                      model)]
        [(not) (not/pc (parse-qexpr (cadr qexpr) model) model)]
        [(lt)  (apply lt/p (append (parse-args (cdr qexpr)) (list model)))]
        [(lte) (apply lte/p (append (parse-args (cdr qexpr)) (list model)))]
        [(eq)  (apply eq/p (append (parse-args (cdr qexpr)) (list model)))]
        [(gt)  (apply gt/p (append (parse-args (cdr qexpr)) (list model)))]
        [(gte) (apply gte/p (append (parse-args (cdr qexpr)) (list model)))]
        [(neq) (apply neq/p (append (parse-args (cdr qexpr)) (list model)))]
        [else  (raise-arguments-error 'query
                                     "can't apply unknown filter"
                                     "filter" (car qexpr))])))

(define (encode-fields model)
  (join ", " (map (lambda (f)
                    (string-append (model-name model) "."
                                   (field-name f)))
                  (model-fields model))))

; query: model -> qexpr -> querystmt
(define (query model [qexpr '()])
  (let ([start (string-append "select "
                              (encode-fields model)
                              " from " (model-name model))])
    (cond [(null? qexpr) (list model start)]
          [else (let* ([querystmt (parse-qexpr qexpr model)]
                       [querystring (cadr querystmt)]
                       [queryparams (cddr querystmt)])
                  (flatten (list model
                                 (string-append start " where " querystring)
                                 queryparams)))])))


(module+ test
         (define cat (model "cat"
                            #t
                            (list (field "age" (plain-field "int"))
                                  (field "color" (plain-field "string")))))
         (print (cdr (query cat '(or (not (lt age 3))
                                     (and (gt age 7) (eq color "red"))))))
         (display "\n"))
