#lang racket

;; Build ddl, queries, insert and delete statements from models. Check
;; them for correctness whenever possible. This module won't be
;; db-agnostic till I get the basics right (I'm starting with
;; sqlite3).
