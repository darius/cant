;; Kernel language
;; e expression
;; p pattern
;; r environment

(define (eval e r)
  (case e
    ({constant value}
     value)
    ({variable name}
     (r name))
    ({make script}
     (object<- script r))               ;XXX elaborate
    ({hide e1}
     (let new-r (env-extend-promises r (exp-vars-defined e1)))
     (eval e1 new-r))
    ({sequence e1 e2}
     (eval e1 new-r)
     (eval e2 new-r))
    ({let p e1}
     (let value (eval e1 r))
     (if (match value p r)
         value
         (fail-match)))
    ({call e1 e2}
     (call (eval e1 r) (eval e2 r)))
    ({list es}
     (for each ((e1 es))
       (eval e1 r)))
    ({term tag e1}
     (term<- tag (eval e1 r)))
    ;; XXX make-trait?
    ))

(define (match subject p r)
  (case p
    ({any-pat}
     #yes)
    ({variable-pat name}
     (env-resolve! r name subject)
     #yes)
    ({constant-pat value}
     (= subject value))
    ({count-pat n}
     (and (list? subject) (= subject.count n)))
    ({prefix-pat ps p-rest}
     (and (list? subject)
          (match-prefix subject ps p-rest r)))
    ({term-pat tag p-args}
     (and (term? subject)
          (match subject.tag tag r)
          (match subject.arguments p-args r)))
    ({view-pat e p1}
     (match (call (eval e r) subject)  ;;XXX (list<- subject)?
            p1 r))
    ))

(define (match-prefix subjects ps p-rest r)
  (and (list? subjects)
       (<= subjects.count ps.count)
       (begin matching ((subjects subjects) (ps ps))
         (if ps.empty?
             (match subjects p-rest r)
             (and (match subjects.first ps.first r)
                  (matching subjects.rest ps.rest))))))

(define (exp-vars-defined e)
  (case e
    ({let p e1}       (pat-vars-defined p))
    ({sequence e1 e2} (chain (exp-vars-defined e1)
                             (exp-vars-defined e2)))
    (_                '())))

(define (pat-vars-defined p)
  (case p
    ({any-pat}
     '())
    ({variable-pat name}
     `(,name))
    ({constant-pat value}
     '())
    ({count-pat n}
     '())
    ({prefix-pat ps p-rest}
     (chain (gather pat-vars-defined ps) (pat-vars-defined p-rest)))
    ({term-pat tag p-args}
     (chain (pat-vars-defined tag) (pat-vars-defined p-args)))
    ({view-pat e p1}
     (pat-vars-defined p1))
    ))
