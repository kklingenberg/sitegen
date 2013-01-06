#lang racket

;; Global definitions.

(provide prep-connection)


; Default connection to the database. Functions that need a database
; connection use this by default by specifying #:conn #f, but can be
; given a different one. This connection will be wrapped in a promise
; for now, but could not be.
(define *default-connection* (delay "asdf")) ; TODO database connection.


; Forces a connection out of a promise. If the connection is a
; promise, it will be evaluated and saved in *default-connection* for
; future application. This loses the previous default connection but
; it'll have to be for now.
(define (prep-connection connection)
  (let ([conn (if connection connection *default-connection*)])
    (if (promise? conn)
        (begin
          (set! *default-connection* (force conn))
          *default-connection*)
        conn)))
