#lang racket

;; Non-query statements.

(require "./model.rkt" "./query.rkt")

(provide delete/stmt delete-from/stmt save-to/stmt
         (contract-out
          [struct stmt ((model model?)
                        (string string?)
                        (params (listof any/c)))]))


; A delete, delete-from or save-to form is interpreted to a stmt.
(struct stmt (model string params))

; delete/stmt: qstmt -> stmt
(define (delete/stmt query)
  (let ([model (qstmt-model query)])
    (stmt model
          (string-append "delete from " (model-name model) (filters query))
          (qstmt-params query))))

; delete-from/stmt: model -> qexpr -> stmt
; delete-from/stmt: model -> hashmap -> stmt
(define (delete-from/stmt model qexpr/hashmap)
  '())

; save-to/stmt: model -> hashmap -> stmt
(define (save-to/stmt model obj)
  '())

(module+ test
         (define-model person
           (field "firstname" (plain-field "string")))
         (define-model cat
           (field "age" (plain-field "int"))
           (field "color" (plain-field "string"))
           (field "owner" (foreign-key person)))

         (display "TESTING database/qly.rkt\n\n")

         (let ([s (delete/stmt (select-from cat '(= color "red")))])
           (print (stmt-string s))
           (print (stmt-params s)))
         (display "\n--\n")

         (let ([s (delete/stmt (select-related 'owner cat))])
           (print (stmt-string s))
           (print (stmt-params s)))
         (display "\n--\n")

         (let ([s (delete/stmt (select-from cat))])
           (print (stmt-string s))
           (print (stmt-params s)))
         (display "\n--\n"))
