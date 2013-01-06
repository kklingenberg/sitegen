#lang racket

(provide zip
         (contract-out
          [join (string? (listof string?) . -> . string?)]
          [lookup (-> (-> any/c boolean?) (listof any/c) any/c)]))


(define (join inter string-list)
  (cond [(null? string-list) ""]
        [else (foldl (lambda (e acc)
                      (string-append acc inter e))
                    (car string-list)
                    (cdr string-list))]))


(define (lookup pred lst)
  (cond [(null? lst) #f]
        [(pred (car lst)) (car lst)]
        [else (lookup pred (cdr lst))]))


; A map that finishes application when the shortest list runs out of
; elements.
(define (zip f l . ls)
  (let ([ls (cons l ls)])
    (cond [(ormap null? ls) '()]
          [else (cons (apply f (map car ls))
                      (apply zip `(,f ,@(map cdr ls))))])))
