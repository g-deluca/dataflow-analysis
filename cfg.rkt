#lang racket

;; Intraprocedure control flow graph, transformed from AST
;; Control flow graph
;; Node represents statement
;; Edge represents a directed control flow

(require "ast.rkt")
(require "parser.rkt")
(require rackunit)

(provide (all-defined-out))

(struct Node (body label) #:transparent)
(struct Edge (from to label) #:transparent)
(struct CFG (entry exit nodes edges) #:transparent)

(define (stmt->cfg stmt)
  (define node-id 1)
  (define edge-id 1)

  (define (make-node body [label node-id])
    (set! node-id (add1 node-id))
    (Node body label))

  (define (make-edge from to [label edge-id])
    (set! edge-id (add1 edge-id))
    (Edge from to label))

  (define (cfg-helper stmt)
    (match stmt
      [(If cnd thn els)
       (define entry (make-node cnd))
       (define thn-cfg (cfg-helper thn))
       (define els-cfg (cfg-helper els))
       (define exit (make-node (Skip)))
       (CFG entry exit
            (append (list entry exit)
                    (CFG-nodes thn-cfg)
                    (CFG-nodes els-cfg))
            (append (list (make-edge entry (CFG-entry thn-cfg))
                          (make-edge entry (CFG-entry els-cfg))
                          (make-edge (CFG-exit thn-cfg) exit)
                          (make-edge (CFG-exit els-cfg) exit))
                    (CFG-edges thn-cfg)
                    (CFG-edges els-cfg)))]
      [(While cnd body)
       (define body-cfg (cfg-helper body))
       (define entry (make-node cnd))
       (define es (list (make-edge entry (CFG-entry body-cfg))
                        (make-edge (CFG-exit body-cfg) entry)))
       (CFG entry entry
            (cons entry (CFG-nodes body-cfg))
            (append es (CFG-edges body-cfg)))]
      [(Seq stmts)
       (define cfgs (map cfg-helper stmts))
       (define entry (CFG-entry (first cfgs)))
       (define exit (CFG-exit (last cfgs)))
       (define conns (for/list ([exit cfgs]
                                [entry (cdr cfgs)])
                       (make-edge (CFG-exit exit) (CFG-entry entry))))
       (define all-nodes (foldl (λ (c acc) (append (CFG-nodes c) acc)) '() cfgs))
       (define all-edges (foldl (λ (c acc) (append (CFG-edges c) acc)) '() cfgs))
       (CFG entry exit all-nodes (append conns all-edges))]
      [else (let ([n (make-node stmt)]) (CFG n n (list n) '()))]))
  (cfg-helper stmt))

(define (fun->cfg f)
  (stmt->cfg (Fun-body f)))

(define (get-succs n cfg)
  (map Edge-to (filter (λ (e) (equal? (Edge-from e) n)) (CFG-edges cfg))))

(define (get-preds n cfg)
  (map Edge-from (filter (λ (e) (equal? (Edge-to e) n)) (CFG-edges cfg))))

(define (get-exprs cfg)
  (define (get-exprs-from-node n)
    (match (Node-body n)
      [(Assign id e) (list e)]
      [(Plus l r) (Plus l r)]
      [(Minus l r) (Minus l r)]
      [(Mult l r) (Mult l r)]
      [(Div l r) (Div l r)]
      [(Greater l r) (Greater l r)]
      [(Equal l r) (Equal l r)]
      [else (list)]))
  (flatten (map get-exprs-from-node (CFG-nodes cfg))))

(module+ test
  (check-match (stmt->cfg (parse-stmt '{:= a 3}))
                (CFG (Node (Assign 'a 3) _)
                     (Node (Assign 'a 3) _)
                     (list (Node (Assign 'a 3) _))
                     '()))

  (check-match (stmt->cfg (parse-stmt '{{:= a 3} {:= b 4}}))
                (CFG
                 (Node (Assign 'a 3) _)
                 (Node (Assign 'b 4) _)
                 (list (Node (Assign 'b 4) _) (Node (Assign 'a 3) _))
                 (list (Edge (Node (Assign 'a 3) _) (Node (Assign 'b 4) _) _))))

  (check-match (stmt->cfg (parse-stmt '{{:= a 1}
                                         {:= b 2}
                                         {:= c 3}}))
                (CFG
                 (Node (Assign 'a 1) _)
                 (Node (Assign 'c 3) _)
                 (list (Node (Assign 'c 3) _) (Node (Assign 'b 2) _) (Node (Assign 'a 1) _))
                 (list (Edge (Node (Assign 'a 1) _) (Node (Assign 'b 2) _) _)
                       (Edge (Node (Assign 'b 2) _) (Node (Assign 'c 3) _) _))))

  (check-match (stmt->cfg (parse-stmt '{{:= a 1}
                                         {if {== a 1} {:= b 2} {:= b 3}}
                                         {:= c 4}}))
                (CFG
                 (Node (Assign 'a 1) _)
                 (Node (Assign 'c 4) _)
                 (list
                  (Node (Assign 'c 4) _)
                  (Node (Equal 'a 1) _)
                  (Node (Skip) _)
                  (Node (Assign 'b 2) _)
                  (Node (Assign 'b 3) _)
                  (Node (Assign 'a 1) _))
                 (list
                  (Edge (Node (Assign 'a 1) _) (Node (Equal 'a 1) _) _)
                  (Edge (Node (Skip) _) (Node (Assign 'c 4) _) _)
                  (Edge (Node (Equal 'a 1) _) (Node (Assign 'b 2) _) _)
                  (Edge (Node (Equal 'a 1) _) (Node (Assign 'b 3) _) _)
                  (Edge (Node (Assign 'b 2) _) (Node (Skip) _) _)
                  (Edge (Node (Assign 'b 3) _) (Node (Skip) _) _))))

  (check-match (stmt->cfg (parse-stmt '{{if {== x 1} {:= a 1} {:= b 2}}}))
                (CFG
                 (Node (Equal 'x 1) _)
                 (Node (Skip) _)
                 (list (Node (Equal 'x 1) _)
                       (Node (Skip) _)
                       (Node (Assign 'a 1) _)
                       (Node (Assign 'b 2) _))
                 (list
                  (Edge (Node (Equal 'x 1) _) (Node (Assign 'a 1) _) _)
                  (Edge (Node (Equal 'x 1) _) (Node (Assign 'b 2) _) _)
                  (Edge (Node (Assign 'a 1) _) (Node (Skip) _) _)
                  (Edge (Node (Assign 'b 2) _) (Node (Skip) _) _))))

  (check-match (stmt->cfg (parse-stmt '{while {> 5 x} {:= x {+ x 1}}}))
                (CFG
                 (Node (Greater 5 'x) _)
                 (Node (Greater 5 'x) _)
                 (list (Node (Greater 5 'x) _)
                       (Node (Assign 'x (Plus 'x 1)) _))
                 (list
                  (Edge (Node (Greater 5 'x) _) (Node (Assign 'x (Plus 'x 1)) _) _)
                  (Edge (Node (Assign 'x (Plus 'x 1)) _) (Node (Greater 5 'x) _) _))))

  (check-match (stmt->cfg (parse-stmt '{while {> 5 x} {{if {== x 3} {:= x 4} {:= x 5}}
                                                        {:= x {- x 1}}}}))
                (CFG
                 (Node (Greater 5 'x) _)
                 (Node (Greater 5 'x) _)
                 (list
                  (Node (Greater 5 'x) _)
                  (Node (Assign 'x (Minus 'x 1)) _)
                  (Node (Equal 'x 3) _)
                  (Node (Skip) _)
                  (Node (Assign 'x 4) _)
                  (Node (Assign 'x 5) _))
                 (list
                  (Edge (Node (Greater 5 'x) _) (Node (Equal 'x 3) _) _)
                  (Edge (Node (Assign 'x (Minus 'x 1)) _) (Node (Greater 5 'x) _) _)
                  (Edge (Node (Skip) _) (Node (Assign 'x (Minus 'x 1)) _) _)
                  (Edge (Node (Equal 'x 3) _) (Node (Assign 'x 4) _) _)
                  (Edge (Node (Equal 'x 3) _) (Node (Assign 'x 5) _) _)
                  (Edge (Node (Assign 'x 4) _) (Node (Skip) _) _)
                  (Edge (Node (Assign 'x 5) _) (Node (Skip) _) _))))

  (let ([cfg (stmt->cfg (parse-stmt '{{if {== x 1} {:= a 1} {:= b 2}}}))])
    (check-match (get-succs (CFG-entry cfg) cfg)
                 (list (Node (Assign 'a 1) _) (Node (Assign 'b 2) _)))
    (check-match (get-preds (CFG-exit cfg) cfg)
                 (list (Node (Assign 'a 1) _) (Node (Assign 'b 2) _))))

  (let ([cfg (stmt->cfg (parse-stmt '{while {> 5 x}
                                            {{if {== x 3}
                                                 {:= x 4}
                                                 {:= x 5}}
                                             {:= x {- x 1}}}}))])
    (check-match (get-succs (CFG-exit cfg) cfg)
                 (list (Node (Equal 'x 3) _)))
    (check-match (get-preds (CFG-exit cfg) cfg)
                 (list (Node (Assign 'x (Minus 'x 1)) _)))
    (check-match (get-succs (CFG-entry cfg) cfg)
                 (list (Node (Equal 'x 3) _))))

  )
