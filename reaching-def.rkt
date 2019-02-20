#lang racket

;; Reaching Definition Analysis

(require "ast.rkt")
(require "parser.rkt")
(require "cfg.rkt")
(require "dfa.rkt")

(require rackunit)

(define reaching-definitions-analysis
  (Analysis
   ; direction
   'forward
   ; init
   (λ (cfg node) (set))
   ; entry fact
   (λ (fun cfg entry) (set))
   ; exit fact
   (λ (fun cfg exit) (set))
   ; gen
   (λ (cfg n)
     (match n
       [(Node (Assign id val) label) (set (cons id label))]
       [else (set)]))
   ; kill
   (λ (cfg n)
     (match n
       [(Node (Assign id val) label)
        (list->set
         (map (λ (x) (cons id (Node-label x)))
              (filter (λ (x) (and (Assign? (Node-body x))
                                  (eq? (Assign-id (Node-body x)) id)
                                  (not (eq? (Node-label x) label))))
                      (CFG-nodes cfg))))]
       [else (set)]))
   ; meet
   set-union))

(define reaching-definitions
  (chaotic-iteration reaching-definitions-analysis))
