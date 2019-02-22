#lang racket

;; Live Variables Analysis
;; A variable is live at a program point if its
;; current value may be read during the remaining
;; execution of program.

(require "ast.rkt")
(require "parser.rkt")
(require "cfg.rkt")
(require "dfa.rkt")

(require rackunit)

(define live-variables-analysis
  (Analysis
   ; direction
   'backward
   ; init
   (λ (cfg n) (set))
   ; entry fact
   (λ (fun cfg entry) (set))
   ; exit fact
   (λ (fun cfg exit) (set))
   ; gen
   (λ (cfg n)
     (match n
       [(Node (Assign id e) _) (get-vars e)]
       [(Node (Equal l r) _)
        (set-union (get-vars l) (get-vars r))]
       [(Node (Greater l r) _)
        (set-union (get-vars l) (get-vars r))]
       [else (set)]))
   ; kill
   (λ (cfg n)
     (match n
       [(Node (Assign id e) _) (set id)]
       [else (set)]))
   ; meet
   set-union))

(define (live-variables-analysis-star final-set)
  (Analysis
   ; direction
   'backward
   ; init
   (λ (cfg n) (set))
   ; entry fact
   (λ (fun cfg entry) (set))
   ; exit fact
   (λ (fun cfg exit) final-set)
   ; gen
   (λ (cfg n)
     (match n
       [(Node (Assign id e) _) (get-vars e)]
       [(Node (Equal l r) _)
        (set-union (get-vars l) (get-vars r))]
       [(Node (Greater l r) _)
        (set-union (get-vars l) (get-vars r))]
       [else (set)]))
   ; kill
   (λ (cfg n)
     (match n
       [(Node (Assign id e) _) (set id)]
       [else (set)]))
   ; meet
   set-union))

(define live-variables
  (chaotic-iteration live-variables-analysis))

(define (live-variables-star final-set)
  (chaotic-iteration (live-variables-analysis-star final-set)))

(define (live-variables-worklist-star final-set)
  (worklist-iteration (live-variables-analysis-star final-set)))


;; Tests
(module+ test
  (define test-stmt
    (parse-stmt
     '{{:= x 2}
       {:= y 4}
       {:= x 1}
       {if {> y x}
           {:= z y}
           {:= z {* y y}}}
       {:= x z}}))

  ; live-variables test
  (printf "Testing live-variables: ")
  (define result (live-variables test-stmt))
  (define result-OUT (cdr result))

  (check-equal? (make-immutable-hash (hash->list result-OUT))
                (hash
                 (Node (Skip) 7) (set 'z)
                 (Node (Greater 'y 'x) 4) (set 'y)
                 (Node (Assign 'x 1) 3) (set 'x 'y)
                 (Node (Assign 'y 4) 2) (set 'y)
                 (Node (Assign 'z 'y) 5) (set 'z)
                 (Node (Assign 'x 2) 1) (set)
                 (Node (Assign 'x 'z) 8) (set)
                 (Node (Assign 'z (Mult 'y 'y)) 6) (set 'z)))
  (printf "OK\n")

  ; live-variables-star test
  (printf "Testing live-variables-star: ")
  (define result-star ((live-variables-star (set 'x 'y' z)) test-stmt))
  (define result-star-OUT (cdr result-star))

  (check-equal? (make-immutable-hash (hash->list result-star-OUT))
                (hash
                 (Node (Greater 'y 'x) 4) (set 'y)
                 (Node (Skip) 7) (set 'y 'z)
                 (Node (Assign 'z 'y) 5) (set 'y 'z)
                 (Node (Assign 'x 2) 1) (set)
                 (Node (Assign 'y 4) 2) (set 'y)
                 (Node (Assign 'z (Mult 'y 'y)) 6) (set 'y 'z)
                 (Node (Assign 'x 'z) 8) (set 'x 'y 'z)
                 (Node (Assign 'x 1) 3) (set 'x 'y)))
  (printf "OK\n")

  ; worklist-iteration test
  (printf "Testing live-variables-worklist-star: ")
  (define result-wl-star ((live-variables-worklist-star (set 'x 'y' z)) test-stmt))
  (define result-wl-star-OUT (cdr result-star))

  (check-equal? (make-immutable-hash (hash->list result-star-OUT))
                (make-immutable-hash (hash->list result-wl-star-OUT)))
  (printf "OK\n")
)
