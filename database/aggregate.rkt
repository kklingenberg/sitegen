#lang racket

;; Aggregate functions applied to queries.

(require db "../settings.rkt" "./model.rkt" "./query.rkt")

(provide count)

; An aggregate function applied to a query would restrict further use
; of it, so the statement is executed right away and the SQL is
; discarded.

; count: db -> qstmt -> number
(define (count #:conn [connection #f] qs)
  (let* ([st (string-append "select count(*) from "
                            (model-name (qstmt-model qs))
                            (filters qs))]
         [conn (prep-connection connection)]
         [conn/st/args (append conn st (qstmt-params qs))])
    (apply query-value conn/st/args)))

; TODO other aggregate functions
