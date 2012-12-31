#lang racket

;; Generates ddl strings for models.

(require "../utils.rkt" "./model.rkt")

(provide ddl
         charfield    intfield  floatfield
         booleanfield datefield datetimefield)


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
                              " autoincrement" ""))]
          [(foreign-key? fieldtype)
           (foreign-key/encode (foreign-key-referenced fieldtype))]
          [(self-key? fieldtype) (foreign-key/encode a-model)]
          [else ""])))


; ddl: model -> string
(define (ddl a-model #:with-fks [with-fks #t])

  (define (field-clause a-field)
    (string-append (field-name a-field) " "
                   (fieldtype/encode a-field a-model with-fks)))

  (string-append "create table " (model-name a-model)
                 " (" (join ", " (map field-clause (model-fields a-model)))
                 ");"))


; common fields
; this oughta be in a sqlite-specific file
(define charfield     (plain-field "varchar"))
(define intfield      (plain-field "int"))
(define floatfield    (plain-field "float"))
(define booleanfield  (plain-field "boolean"))
(define datefield     (plain-field "date"))
(define datetimefield (plain-field "datetime"))


(module+ test
         (define-model cat
           (field "age" (plain-field "int"))
           (field "color" (plain-field "string"))
           (field "father" (self-key)))
         (define-model (bowl #f)
           (field "owner" (foreign-key cat))
           (field "size" (plain-field "int")))

         (display "TESTING database/ddl.rkt\n\n")

         (display cat)
         (display "\n--\n")
         (display (ddl cat))
         (display "\n--\n")
         (display bowl)
         (display "\n--\n")
         (display (ddl bowl))
         (display "\n\nTest for cyclical reference\n\n")
         (define-model A
           (field "c_rel" (foreign-key C)))
         (define-model B
           (field "a_rel" (foreign-key A)))
         (define-model C
           (field "b_rel" (foreign-key B)))
         (display (ddl A))
         (display "\n--\n")
         (display (ddl B))
         (display "\n--\n")
         (display (ddl C))
         (display "\n------------\n"))
