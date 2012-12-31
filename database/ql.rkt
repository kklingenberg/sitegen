#lang racket

;; Build ddl, queries, insert and delete statements from models. Check
;; them for correctness whenever possible.

(require "./model.rkt" "./query.rkt")

(provide delete delete-from save-to
         (contract-out
          [struct stmt ((model model?)
                        (string string?)
                        (params (listof any/c)))]))


; A delete, delete-from or save-to form is interpreted to a stmt.
(struct stmt (model string params))

; delete: qstmt -> stmt
(define (delete query)
  (let ([model (qstmt-model query)])
    (stmt model
          (string-append "delete from " (model-name model) (filters query))
          (qstmt-params query))))

; delete-from: model -> qexpr -> stmt
; delete-from: model -> hashmap -> stmt
(define (delete-from model qexpr/hashmap)
  '())

; save-to: model -> hashmap -> stmt
(define (save-to model obj)
  '())

(module+ test
         (define-model person
           (field "firstname" (plain-field "string")))
         (define-model cat
           (field "age" (plain-field "int"))
           (field "color" (plain-field "string"))
           (field "owner" (foreign-key person)))

         (display "TESTING database/qly.rkt\n\n")

         (let ([s (delete (select-from cat '(= color "red")))])
           (print (stmt-string s))
           (print (stmt-params s)))
         (display "\n--\n")

         (let ([s (delete (select-related 'owner cat))])
           (print (stmt-string s))
           (print (stmt-params s)))
         (display "\n--\n")

         (let ([s (delete (select-from cat))])
           (print (stmt-string s))
           (print (stmt-params s)))
         (display "\n--\n"))
