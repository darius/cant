(library (terp macros)
(export unpack define-enum mcase mlambda)
(import (chezscheme))

(define-syntax unpack
  (syntax-rules ()
    ((_ vec vars exp1 exp2 ...)
     (let ((t vec))
       (vector-let-each vars 1 t (begin exp1 exp2 ...))))))

(define-syntax vector-let-each
  (syntax-rules ()
    ((_ () _ _ body)
     body)
    ((_ (var . vars) i t body)
     (let ((var (vector-ref t i)))
       (vector-let-each vars (+ i 1) t body)))))

(define-syntax define-enum
  (syntax-rules ()
    ((_ var ...)
     (define-enum-from 0 var ...))))

(define-syntax define-enum-from
  (syntax-rules ()
    ((_ k)
     (begin))
    ((_ k var var1 ...)
     (begin (define var k) (define-enum-from (+ k 1) var1 ...)))))

(define-syntax mcase
  (syntax-rules ()
    ((_ subject clause ...)
     ((mlambda clause ...) subject))))

(define-syntax mlambda
  (syntax-rules ()
    ((_ clause ...)
     (lambda (subject)
       (%match-clauses subject clause ...)))))

(define-syntax %match-clauses
  (syntax-rules ()
    ((_ subject)
     (%match-error subject))
    ((_ subject (pattern) . clauses)
     (let ((match-rest (lambda ()
                         (%match-clauses subject . clauses))))
       (%match subject pattern #t (match-rest))))
    ((_ subject (pattern . body) . clauses)
     (let ((match-rest (lambda ()
                         (%match-clauses subject . clauses))))
       (%match subject pattern (begin . body) (match-rest))))))
    
(define (%match-error subject)
  (error 'mcase "Match failure" subject))

(define-syntax %match
  (syntax-rules (__ : quote)                    ;N.B. __ was _
    ((_ subject (: __ ok?) then-exp else-exp)
     (if (ok? subject)
         then-exp
         else-exp))
    ((_ subject (: var ok?) then-exp else-exp)
     (if (ok? subject)
         (let ((var subject)) then-exp)
         else-exp))
    ((_ subject (quote datum) then-exp else-exp)
     (if (equal? subject (quote datum)) then-exp else-exp))
    ((_ subject () then-exp else-exp)
     (if (null? subject) then-exp else-exp))
    ((_ subject (h . t) then-exp else-exp)
     (if (pair? subject)
         (mcase (car subject)
                (h (mcase (cdr subject)
                          (t then-exp)
                          (__ else-exp)))
                (__ else-exp))
         else-exp))
    ((_ subject variable then-exp else-exp) ;treating a variable as the only case left
     (let ((variable subject)) then-exp))
    ;; In Gambit we had a case for other constants, like numbers, but
    ;; we're not using it and I don't see how to implement it in
    ;; syntax-rules anyway.
    ))

)
