#lang racket

;; Parser of TIP language, converts s-exp to AST

(require rackunit)
(require "ast.rkt")

(provide (all-defined-out))

; sexp -> Stmt
(define (parse-stmt s)
  (match s
    [`{:= ,id ,e} (Assign (parse-expr id) (parse-expr e))]
    [`{if ,cnd ,thn ,els} (If (parse-expr cnd)
                              (parse-stmt thn)
                              (parse-stmt els))]
    [`{while ,cnd ,body} (While (parse-expr cnd)
                                (parse-stmt body))]
    [`{skip} (Skip)]
    [`{,stmts ...} (Seq (map parse-stmt stmts))]
    [else (error 'parse-stmt "can not parse statement")]))

; sexp -> Expr
(define (parse-expr e)
  (match e
    [(? symbol? s) s]
    [(? integer? i) i]
    [`{+ ,lhs ,rhs} (Plus (parse-expr lhs)
                          (parse-expr rhs))]
    [`{- ,lhs ,rhs} (Minus (parse-expr lhs)
                           (parse-expr rhs))]
    [`{* ,lhs ,rhs} (Mult (parse-expr lhs)
                          (parse-expr rhs))]
    [`{/ ,lhs ,rhs} (Div (parse-expr lhs)
                         (parse-expr rhs))]
    [`{> ,lhs ,rhs} (Greater (parse-expr lhs)
                             (parse-expr rhs))]
    [`{== ,lhs ,rhs} (Equal (parse-expr lhs)
                            (parse-expr rhs))]
    [else (error 'parse-expr "can not parse expression")]))

; sexp -> Fun
; (define (parse-function f)
;   (match f
;     [`{,fname {,vars ...} {var ,locals ...} ,stmts}
;      (Fun fname vars locals (parse-stmt stmts))]
;     [`{,fname {,vars ...} {} ,stmts}
;      (Fun fname vars '() (parse-stmt stmts))]
;     [else (error 'parse-function "can not parse function")]))

;;;;;;;;;;;;;;;;;

(module+ test

  (check-equal? (parse-stmt '{{:= a 3}})
                (Seq (list (Assign 'a 3))))

  (check-equal? (parse-stmt '{{:= a 3} {:= b 4} {:= c {+ a b}}})
                (Seq (list (Assign 'a 3) (Assign 'b 4) (Assign 'c (Plus 'a 'b)))))

  (check-equal? (parse-stmt '{while {> x 0}
                                    {:= x {- x 1}}})
                (While (Greater 'x 0) (Assign 'x (Minus 'x 1))))
)
