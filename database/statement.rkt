#lang racket

;; Non-query statements.

(require "../utils.rkt" "./model.rkt" "./query.rkt")

(provide delete/stmt delete-from/stmt insert-into/stmt update/stmt
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
  (define (filters-from-hash h)
    (let ([keys (hash-keys h)])
      (qstmt model
             (string-append "(" (join " and "
                                      (map (lambda (k)
                                             (string-append (model-name model)
                                                            "." k " = ?"))
                                           keys)) ")")
             (map (lambda (k) (hash-ref h k)) keys))))
  (let ([filters (cond [(hash? qexpr/hashmap) (filters-from-hash qexpr/hashmap)]
                       [else (parse-qexpr qexpr/hashmap model)])])
    (stmt model
          (string-append "delete from " (model-name model)
                         " where " (qstmt-qstring filters))
          (qstmt-params filters))))

; insert-into/stmt: model -> hashmap -> stmt
(define (insert-into/stmt model obj)
  '())

; update/stmt: model -> hashmap -> stmt
(define (update/stmt model obj)
  '())


(module+ test
         (define-model person
           (field "firstname" (plain-field "string")))
         (define-model cat
           (field "age" (plain-field "int"))
           (field "color" (plain-field "string"))
           (field "owner" (foreign-key person)))

         (display "TESTING database/statement.rkt\n\n")

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
         (display "\n--\n")

         (let ([s (delete-from/stmt cat '(and (= age 3) (= color "brown")))])
           (print (stmt-string s))
           (print (stmt-params s)))
         (display "\n--\n")

         (let ([s (delete-from/stmt cat #hash(("age" . 3) ("color" . "brown")))])
           (print (stmt-string s))
           (print (stmt-params s)))
         (display "\n--\n"))

