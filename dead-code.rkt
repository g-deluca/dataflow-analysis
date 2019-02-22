#lang racket

(require "ast.rkt")
(require "parser.rkt")
(require "cfg.rkt")
(require "dfa.rkt")
(require "live-var.rkt")

(require rackunit)

;;;;;;;;;;;
;; Todo esto lo hago porque trabajar con hashes en racket está bien feo

(define (is-first elem)
  (match elem
    [(Node _ 1) #t]
    [else #f]))

; toma un hash y te devuelve el conjunto de variables vivas a la entrada
; del primer nodo
(define (get-live-first hs)
  (define hss (filter (lambda (elem) (is-first (car elem))) (hash->list hs)))
  ; siempre hay un único elemento que satisface is-first por lo tanto puedo
  ; hacer esto:
  (cdr (first hss)))

;;;;;;;;;;;

(define (live prog o)
  (define live-vars ((live-variables-worklist-star o) prog))
  (define input-live-vars (car live-vars))
  (get-live-first input-live-vars))

(define (bury prog o)
  (match prog
    [(Skip) (Skip)]
    [(Assign x a) (cond [(set-member? o x) (Assign x a)] [else (Skip)])]
    [(Seq s ) (Seq (buryList s o))]
    [(If cnd thn els) (If cnd (bury thn o) (bury els o))]
    [(While cnd body) (While cnd (bury body (live (While cnd body) o)))]
  )
)

(define (buryList stmts o)
  (match stmts
    [(cons x xs) #:when (> (length xs) 0)
                        (define ox (live (Seq xs) o))
                        (flatten (list (bury x ox) (buryList xs o)))]
    [(cons x xs) #:when (= (length xs) 0)
                           (bury x o)]
    [else '()]))
    ; uso guardas porque no sé hacer pattern matching en racket (:

(module+ test
 (define test-stmt-1
   (parse-stmt
    '{{:= x 2}
      {:= y 4}
      {:= x 1}
      {if {> y x}
          {{:= z y}
           {:= z 5}}
          {{:= z {* y y}}
           {:= y 2}}}
      {:= x z}}))

 (define test-stmt-2
   (parse-stmt
    '{{:= x {+ a b}}
      {:= y {* a b}}
      {while {> y {+ a b}}
             {{:= a {+ a 1}}
              {:= b {+ b 5}}
              {:= b 3}
              {:= x {+ a b}}
              {:= a 4}}}
      }))

 (define test-stmt-3
   (parse-stmt
    '{{:= b 0}
      {while {> 10 {* x b}}
             {{if {> 5 y}
                  {{:= y {+ y 1}}
                   {:= a {* a y}}
                   {:= x {+ x 1}}
                   {:= x {+ y 2}}}
                  {{:= x {+ x 1}}
                   {:= a {+ a x}}
                   {skip}
                   {:= b 2}}}
              {:= b 1}}}
      }))

  (define test-stmt-4
    (parse-stmt
      '{{if {> x 101}
           {{:= a 33}{:= a 42}}
           {{:= b 51}{:= b 42}}}
        {:= c {* a b}}
       }))


 (printf "Test 1: ")
 (define test1 (bury test-stmt-1 (set 'x 'y 'z)))
 (check-equal? test1
               (Seq (list
                      (Skip)
                      (Assign 'y 4)
                      (Assign 'x 1)
                      (If
                       (Greater 'y 'x)
                       (Seq (list (Skip) (Assign 'z 5)))
                       (Seq (list (Assign 'z (Mult 'y 'y)) (Assign 'y 2))))
                      (Assign 'x 'z))))
 (printf "OK\n")

 (printf "Test 2: ")
 (define test2 (bury test-stmt-2 (set 'x 'y 'a 'b)))
 (check-equal? test2
               (Seq (list
                      (Skip)
                      (Assign 'y (Mult 'a 'b))
                      (While
                       (Greater 'y (Plus 'a 'b))
                       (Seq (list
                         (Assign 'a (Plus 'a 1))
                         (Skip)
                         (Assign 'b 3)
                         (Skip)
                         (Assign 'a 4)))))))
 (printf "OK\n")

 (printf "Test 3: ")
 (define test3 (bury test-stmt-3 (set 'x 'y 'a 'b)))
 (check-equal? test3
               (Seq (list
                      (Assign 'b 0)
                      (While
                       (Greater 10 (Mult 'x 'b))
                       (Seq (list
                         (If
                          (Greater 5 'y)
                          (Seq (list
                            (Assign 'y (Plus 'y 1))
                            (Assign 'a (Mult 'a 'y))
                            (Skip)
                            (Assign 'x (Plus 'y 2))))
                          (Seq (list
                                 (Assign 'x (Plus 'x 1))
                                 (Assign 'a (Plus 'a 'x))
                                 (Skip)
                                 (Skip))))
                         (Assign 'b 1)))))))

 (printf "OK\n")

 (printf "Test 4: ")
 (define test4 (bury test-stmt-4 (set 'x 'a 'b)))
 (check-equal? test4
               (Seq (list
                 (If
                  (Greater 'x 101)
                  (Seq (list (Skip) (Assign 'a 42)))
                  (Seq (list (Skip) (Assign 'b 42))))
                 (Skip))))
 (printf "OK\n")
 )
