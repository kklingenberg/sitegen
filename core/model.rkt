#lang racket

;; Models and fields, representing the entities of the site. These
;; entities are the things you display, serialize, build forms for,
;; save to database, etc. sitegen tries to provide facilities to do
;; all those.

;; The actual model instances (e.g. the rows in each database table)
;; will probably end up being simple mutable hash maps.

; field type constructors
; sql encoding is ql's job

(struct plain-field (type) #:transparent)

(struct primary-key (auto-increment) #:transparent)

; If a foreign key specifies *other-model* as a model that doesn't
; have a primary key, an error is thrown.
(struct foreign-key (other-model)
        #:transparent
        #:guard (lambda (other-model type-name)
                  (if (has-pk? other-model)
                      other-model
                      (error type-name
                             "foreign-key to model without primary key: ~a"
                             (model-name other-model)))))

; A self key is a foreign key that references the same model it
; belongs to. Just like the foreign key, the model that has a self key
; should have a primary key. Sadly, I don't know how to enforce this
; on the struct constructor, so an error would remain unchecked until
; the ddl generator attempts to process the model (and then the error
; message is a bit cryptic).
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

(define (has-pk? a-model)
  (one-is-pk? (model-fields a-model)))

(define (get-pk a-model)
  (define (get-pk-iter fields)
    (cond [(null? fields) #f]
          [(primary-key? (field-type (car fields))) (car fields)]
          [else (get-pk-iter (cdr fields))]))
  (get-pk-iter (model-fields a-model)))


(provide (contract-out
          [struct plain-field ((type string?))]
          [struct primary-key ((auto-increment boolean?))]
          [struct foreign-key ((other-model model?))]
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
          [get-pk (-> model? (or/c field? boolean?))]))


(module+ test
         (define cat (model "cat"
                            #t
                            (list (field "age" (plain-field "int"))
                                  (field "color" (plain-field "string")))))
         (define bowl (model "bowl"
                             #f
                             (list (field "owner" (foreign-key cat))
                                   (field "size" (plain-field "float")))))
         (print cat)
         (display "\nhas-pk?: ")
         (display (has-pk? cat))
         (display "\n")
         (print bowl)
         (display "\nhas-pk?: ")
         (display (has-pk? bowl))
         (display "\n"))
