#lang racket

;; Global definitions.

(provide *default-connection*)

; Default connection to the database. Functions that need a database
; connection use this by default, but can be given a different
; one. This connection will be wrapped in a promise for now, but could
; not be.
(define *default-connection* (delay "asdf")) ; TODO database connection.
