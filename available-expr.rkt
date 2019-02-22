#lang racket

;; Available Expressions Analysis
;; An expression is available at a program point if
;; its current value has already been computed earlier
;; in the execution, and not changed on all paths to
;; this program point; so there is no need to re-evaluate
;; it.

(require "ast.rkt")
(require "parser.rkt")
(require "cfg.rkt")
(require "dfa.rkt")

(require rackunit)

(define avail-expr-analysis
  (Analysis
   ; direction
   'forward
   ; init
   (λ (cfg node) (list->set (filter not-constant? (get-exprs cfg))))
   ; entry fact
   (λ (fun cfg n) (set))
   ; exit fact
   null
   ; gen
   (λ (cfg n)
     (match n
       [(Node (Assign id e) _)
        (if (not (expr-contains-var? e id))
            (set e) (set))]
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

(define availble-expr (chaotic-iteration avail-expr-analysis))
(define availble-expr-wl (worklist-iteration avail-expr-analysis))

; Tests
(define test-stmt
  (parse-stmt '{{:= x {+ a b}}
                {:= y {* a b}}
                {while {> y {+ a b}}
                       {{:= a {+ a 1}}
                        {:= x {+ a b}}}}
                }))

; available-expr test
(printf "Testing available-expr: ")
(define result (availble-expr test-stmt))
(define result-IN (car result))

(check-equal? (make-immutable-hash (hash->list result-IN))
              (hash
               (Node (Assign 'a (Plus 'a 1)) 4)
               (set (Plus 'a 'b))
               (Node (Assign 'x (Plus 'a 'b)) 1)
               (set)
               (Node (Assign 'x (Plus 'a 'b)) 5)
               (set)
               (Node (Greater 'y (Plus 'a 'b)) 3)
               (set (Plus 'a 'b))
               (Node (Assign 'y (Mult 'a 'b)) 2)
               (set (Plus 'a 'b))))
(printf "OK\n")

; available-expr-wl test
(printf "Testing available-expr-wl: ")
(define result-wl (availble-expr-wl test-stmt))
(define result-wl-IN (car result))

(check-equal? (make-immutable-hash (hash->list result-IN))
              (make-immutable-hash (hash->list result-wl-IN)))
(printf "OK\n")
