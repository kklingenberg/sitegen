#lang racket

;; Build ddl, queries, insert and delete statements from models. Check
;; them for correctness whenever possible.

(require db "../settings.rkt" "./statement.rkt" "./model.rkt" "./ddl.rkt")

(provide save-to delete delete-from
         make-table
         do-transaction
         all slice get)


; Makes a statement for effects.
(define (make-stmt proc)
  (lambda (#:conn [connection #f] . args)
    (let* ([stmt (apply proc args)]
           [conn (prep-connection connection)]
           [conn/st/args (append conn (stmt-string stmt) (stmt-params stmt))])
      (apply query-exec conn/st/args))))

(define delete      (make-stmt delete/stmt))
(define delete-from (make-stmt delete-from/stmt))

; If the object exists in database, it'll be updated. If not if will
; be inserted.
(define (save-to #:conn [connection #f] model obj)
  '())


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
(define (all qstmt)
  '())

; Limits the result rows with a sql LIMIT modifier, and returns them.
(define (slice limits qstmt)
  '())

; Gets a single row. If the query returns more than one row, an
; exception is thrown.
(define (get qstmt)
  '())
