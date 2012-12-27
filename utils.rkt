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


(define (lookup pred col)
  (cond [(null? col) #f]
        [(pred (car col)) (car col)]
        [else (lookup pred (cdr col))]))
