#lang racket

;; Build ddl, queries, insert and delete statements from models. Check
;; them for correctness whenever possible.

(require db
         "../settings.rkt" "./model.rkt"
         "./query.rkt" "./statement.rkt" "./ddl.rkt" "./aggregate.rkt")

(provide save-to insert-into update delete delete-from ; effects
         make-table                                    ; ddl
         do-transaction                                ; atomic operations
         all slice get                                 ; queries
         count)                                        ; aggregate functions


; Makes a statement for effects.
(define (make-stmt proc)
  (lambda (#:conn [connection #f] . args)
    (let* ([stmt (apply proc args)]
           [conn (prep-connection connection)]
           [conn/st/args (append `(,conn ,(stmt-string stmt))
                                 (stmt-params stmt))])
      (apply query-exec conn/st/args))))

(define delete      (make-stmt delete/stmt))
(define delete-from (make-stmt delete-from/stmt))
(define insert-into (make-stmt insert-into/stmt))
(define update      (make-stmt update/stmt))

; If the object exists in database, it'll be updated. If not if will
; be inserted. Models without primary key will never cause *save-to*
; to update a row. For those models, use the *update* procedure.
(define (save-to #:conn [connection #f] model obj)
  (define (check-row pk-field)
    (let ([qexpr `(= ,(string->symbol (field-name pk-field))
                     ,(hash-ref obj (field-name pk-field)))])
      (case (count #:conn connection (select-from model qexpr))
        [(0)  insert-into/stmt]
        [else (lambda (m o) (update/stmt m o qexpr))])))
  (let* ([pk (get-pk model)]
         [op (cond [(and pk (hash-ref obj (field-name pk) #f)) (check-row pk)]
                   [else insert-into/stmt])])
    ((make-stmt op) #:conn connection model obj)))


; Attempts to create a table for the specified model.
(define (make-table a-model
                    #:with-fks [with-fks #t]
                    #:conn [connection #f])
  (let ([conn (prep-connection connection)]
        [st (ddl a-model #:with-fks with-fks)])
    (unless (table-exists? conn (model-name a-model))
      (query-exec conn st))))


; Wraps its body in a transaction. It commits if the last statement is
; executed successfully, and rolls back if an exception is caught.
(define (do-transaction proc #:conn [connection #f])
  (call-with-transaction (prep-connection connection) proc))


; Gets all the result rows of executing the query.
(define (all #:conn [connection #f] qs)
  (define fieldnames (map field-name (model-fields (qstmt-model qs))))
  (define (hashify v)
    ; this works only because the field order is preserved in the
    ; query and therefore in the result vector too
    (make-immutable-hash (map cons fieldnames (vector->list v))))
  (let* ([conn (prep-connection connection)]
         [conn/st/args (append `(,conn ,(qstmt-qstring qs))
                               (qstmt-params qs))])
    (map hashify (apply query-rows conn/st/args))))


; Limits the result rows with sql LIMIT and OFFSET modifiers, and
; returns them.
(define (slice #:conn [connection #f] #:offset offset #:total total qs)
  (let* ([limits (string-append " limit " (number->string total)
                                " offset " (number->string offset))]
         [limited (qstmt (qstmt-model qs)
                         (string-append (qstmt-qstring qs) limits)
                         (qstmt-params qs))])
    (all #:conn connection limited)))


; Gets a single row. If the query returns more than one row or zero
; rows, an exception is thrown.
(define (get #:conn [connection #f] qs)
  (let ([result (all #:conn connection qs)])
    (cond [(null? result) (raise-result-error "get" "1 row" "0 rows")]
          [(not (null? (cdr result)))
           (raise-result-error "get" "1 row" "more than 1 row")]
          [else (car result)])))
