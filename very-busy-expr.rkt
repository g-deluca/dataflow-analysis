#lang racket

;; Very Busy Expression Analysis
;; An expression is very busy if, no matter what
;; path is taken, it will definitely be evaluated
;; again before its value changes.

(require "ast.rkt")
(require "parser.rkt")
(require "cfg.rkt")
(require "dfa.rkt")

(require rackunit)

(define very-busy-analysis
  (Analysis
   ; direction
   'backward
   ; init
   (λ (cfg node) (list->set (filter not-constant? (get-exprs cfg))))
   ; entry fact
   null
   ; exit fact
   (λ (fun cfg n) (set))
   ; gen
   (λ (cfg n)
     (match n
       [(Node (Assign id e) _)
        (if (not-constant? e) (set e) (set))]
       [else (set)]))
   ; kill
   (λ (cfg n)
     (match n
       [(Node (Assign id e) _)
        (list->set (filter (λ (e1) (expr-contains-var? e1 id))
                           (set->list (get-exprs cfg))))]
       [else (set)]))
   ; meet
   set-intersect
   ))

(define very-busy-exprs
  (chaotic-iteration very-busy-analysis))
