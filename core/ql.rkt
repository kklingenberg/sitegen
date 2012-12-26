#lang racket

;; Build ddl, queries, insert and delete statements from models. Check
;; them for correctness whenever possible. This module won't be
;; db-agnostic till I get the basics right (I'm starting with
;; sqlite3).

(require "../utils.rkt" "model.rkt")

(provide ddl)


(define (fieldtype/encode a-field a-model fks?)

  (define (foreign-key/encode target-model)
    (let ([other-model-name (model-name target-model)]
          [other-model-pk-name (field-name (get-pk target-model))])
      (string-append "int"
                     (if fks?
                         (string-append ", foreign key(" (field-name a-field)
                                        ") references " other-model-name
                                        "(" other-model-pk-name ")")
                         ""))))

  (let ([fieldtype (field-type a-field)])
    (cond [(plain-field? fieldtype) (plain-field-type fieldtype)]
          [(primary-key? fieldtype)
           (string-append "int primary key"
                          (if (primary-key-auto-increment fieldtype)
                              " autoincrement"
                              ""))]
          [(foreign-key? fieldtype)
           (foreign-key/encode (foreign-key-other-model fieldtype))]
          [(self-key? fieldtype) (foreign-key/encode a-model)]
          [else ""])))


; ddl: model -> string
(define (ddl a-model #:with-fks [with-fks #t])

  (define (field-clause a-field)
    (string-append (field-name a-field)
                   " "
                   (fieldtype/encode a-field a-model with-fks)))

  (string-append "create table " (model-name a-model)
                 " (" (join ", " (map field-clause (model-fields a-model)))
                 ");"))


(module+ test
         (define cat (model "cat" #t
                            (list (field "age" (plain-field "int"))
                                  (field "color" (plain-field "string"))
                                  (field "father" (self-key)))))
         (display cat)
         (display "\n--\n")
         (display (ddl cat))
         (display "\n"))
