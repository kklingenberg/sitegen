#lang racket

(provide (contract-out
          [join (string? (listof string?) . -> . string?)]))


(define (join inter string-list)
  (cond [(null? string-list) ""]
        [else (foldl (lambda (e acc)
                      (string-append acc inter e))
                    (car string-list)
                    (cdr string-list))]))
