;; Solve mostly-linear systems of equations, a la Van Wyk's IDEAL.

;; Constraints are equations between expressions. We represent one as
;; an expression, with '=0' implicit. We try to reduce each expression
;; to a linear combination of variables plus a constant, then
;; eliminate one of the variables, and continue. Nonlinear expressions
;; get put off to try again later.

(import (use "lib/queue")
  empty empty?
  push extend
  peek)

(let ratio-m (use "lib/ratio"))
(import ratio-m ratio<-)

(let (r+ r- r* r/) (each ratio-m '(+ - * /)))
(let r-cmp (ratio-m 'compare))

;; Try to determine as many variables as we can by elimination.
;; Return 'inconsistent if we noticed a contradiction, 'done if we
;; discharged all of the constraints, or else 'stuck. Done does not
;; imply that we determined all of the variables, note. If
;; inconsistent, we still try to determine what we can of the rest.
(to (solve equations)
  (begin solving ((inconsistent? #no) ; Noticed a contradiction yet?
                  (progress? #no)     ; Any progress since the last assessment?
                  (agenda (push (extend empty equations) 'assessment)))
    (match (peek agenda)
      ({nonempty task pending}
       (match task
         ('assessment
          (if progress?
              (solving inconsistent? #no (push pending 'assessment))
              (case (inconsistent?    'inconsistent)
                    ((empty? pending) 'done)
                    (else             'stuck))))
         ({equation defaulty? expr}
          (match (eval-expr expr)       ;XXX better name?
            ('nonlinear
             (solving inconsistent? progress? (push pending task)))
            (combo
             (let terms (expand combo))
             (case ((varies? terms)
                    (eliminate-a-variable terms)
                    (solving inconsistent? #yes pending))
                   ((or defaulty? (zeroish? (constant terms)))
                    ;; The equation was either only a default (whose
                    ;; inconsistency is allowed) or it reduced to an
                    ;; uninformative 0=0: drop it.
                    (solving inconsistent? #yes pending))
                   (else
                    (format "Inconsistent: ~w\n" combo)
                    (solving #yes progress? pending)))))))))))

;; TODO lots more
