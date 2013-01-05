#lang racket

(provide (contract-out
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
