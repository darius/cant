;; MCASE and MLAMBDA as Gambit-Scheme macros.

(define-macro (mlambda . clauses)
  (let ((param (gensym)))
    `(lambda (,param) ,(expand-mlambda param clauses))))

(define-macro (mcase subject-exp . clauses)
  `((mlambda . ,clauses) ,subject-exp))
