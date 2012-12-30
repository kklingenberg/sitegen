#lang racket

;; Models and fields, representing the table in the database.

;; The actual model instances (i.e. the rows in each database table)
;; will probably end up being simple mutable hash maps.

(require "../utils.rkt")

; field type constructors

(struct plain-field (type) #:transparent)

(struct primary-key (auto-increment) #:transparent)

; Foreign keys will be lazy and checked at runtime.
(struct foreign-key-base (referenced) #:transparent)

(define-syntax-rule (foreign-key referenced)
  (foreign-key-base (delay referenced)))

(define (foreign-key-referenced fk)
  (force (foreign-key-base-referenced fk)))

(define foreign-key? foreign-key-base?)

; A self key is a foreign key that references the same model it
; belongs to.
(struct self-key () #:transparent)


(struct field (name type) #:transparent)

(define (one-is-pk? fields)
  (ormap (lambda (f)
           (primary-key? (field-type f)))
         fields))

; If *force-id* is not ``#f`` and there isn't at least one field in
; the list of fields that's a primary key, the constructor adds one to
; the head of the list like so: ``(field "id" (primary-key #t))``
(struct model (name force-id fields)
        #:transparent
        #:guard (lambda (name force-id fields type-name)
                  (cond [(not force-id) (values name force-id fields)]
                        [(one-is-pk? fields) (values name force-id fields)]
                        [else (values name force-id
                                      (cons (field "id" (primary-key #t))
                                            fields))])))

(define-syntax define-model
  (syntax-rules ()
    [(define-model (name) field fields ...)
     (define name (model (symbol->string (quote name)) #t
                         (list field fields ...)))]
    [(define-model (name force-id) field fields ...)
     (define name (model (symbol->string (quote name)) force-id
                         (list field fields ...)))]
    [(define-model name field fields ...)
     (define name (model (symbol->string (quote name)) #t
                         (list field fields ...)))]))

(define (has-pk? a-model)
  (one-is-pk? (model-fields a-model)))

(define (get-pk a-model)
  (lookup (compose primary-key? field-type) (model-fields a-model)))

(define (get-field name a-model)
  (lookup (lambda (f) (equal? name (field-name f))) (model-fields a-model)))


(provide define-model foreign-key foreign-key?
         (contract-out
          [struct plain-field ((type string?))]
          [struct primary-key ((auto-increment boolean?))]
          [struct self-key ()]
          [struct field ((name string?)
                         (type (or/c plain-field?
                                     primary-key?
                                     foreign-key?
                                     self-key?)))]
          [struct model ((name string?)
                         (force-id boolean?)
                         (fields (listof field?)))]
          [has-pk? (-> model? boolean?)]
          [get-pk (-> model? (or/c field? boolean?))]
          [get-field (-> string? model? (or/c field? boolean?))]
          [foreign-key-referenced (-> foreign-key? model?)]))


(module+ test
         (define-model cat
           (field "age" (plain-field "int"))
           (field "color" (plain-field "string")))
         (define-model (bowl #f)
           (field "owner" (foreign-key cat))
           (field "size" (plain-field "float")))
         (display "TESTING core/model.rkt\n\n")
         (print cat)
         (display "\nhas-pk?: ")
         (display (has-pk? cat))
         (display "\nwhich?: ")
         (display (get-pk cat))
         (display "\n")
         (print bowl)
         (display "\nhas-pk?: ")
         (display (has-pk? bowl))
         (display "\nshow the bowl-size: ")
         (display (get-field "size" bowl))
         (display "\n------------\n"))
