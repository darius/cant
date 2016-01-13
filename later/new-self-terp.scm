;; Kernel language
;; e expression
;; p pattern
;; r environment

(define (script<- stamp trait clauses)
  (when stamp (assert (stamp? stamp)))
  (assert (trait? trait))
  (make script
    ({.receive message parent-r}
     (begin matching ((clauses clauses))
       (match clauses
         (()
          (delegate trait (actor<- script parent-r) message))
         (((pattern body) @rest)
          (let r (env-extend parent-r (chain (pat-vars-defined pattern)
                                             (exp-vars-defined body))))
          (if (eval-match message pattern r)
              (eval body r)
              (matching rest))))))
    ({.verify alleged-stamp}
     ;;XXX this is probably crap; figure it out
     (= stamp alleged-stamp))))

(define (actor<- script r)
  (make _
    (message
     (script .receive message r))))

(define (delegate trait self message)
  XXX)

(define (eval e r)
  (match e
    ({constant value}
     value)
    ({variable name}
     (r name))
    ({make e-stamp e-trait clauses}
     (actor<- (script<- (eval e-stamp (parent-only r))
                        (eval e-trait (parent-only r))
                        clauses)
              r))
    ({do e1 e2}
     (eval e1 r)
     (eval e2 r))
    ({let p e1}
     (let value (eval e1 r))
     (if (eval-match value p r)
         value
         (error "Match failure" p value)))
    ({call e1 e2}
     (call (eval e1 r) (eval e2 r)))
    ({list es}
     (for each ((e1 es))
       (eval e1 r)))
    ({term tag e1}
     (term<- tag (eval e1 r)))
    ;; XXX make-trait?
    ))

;; Enforce that only r's parent is accessed. In practice this'd be a
;; static analysis. Later on, this arbitrary implementation
;; restriction might be dropped.
(define (parent-only r)
  (make _
    ((name)
     (assert (not (r .inner? name)))
     (r name))
    ({.bind name value}
     (assert #no))
    ({.inner? name}
     #no)                               ;I guess
    (message (call r message))))

(define (eval-match subject p r)
  (match p
    ({any-pat}
     #yes)
    ({variable-pat name}
     (r .bind name subject)
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
          (eval-match subject.tag tag r)
          (eval-match subject.arguments p-args r)))
    ({and-pat p1 p2}
     (and (eval-match subject p1 r)
          (eval-match subject p2 r)))
    ({view-pat e p1}
     (eval-match (call (eval e (parent-only r))
                       `(,subject))  ;;XXX or just subject?
                 p1 r))))

(define (match-prefix subjects ps p-rest r)
  (and (list? subjects)
       (<= subjects.count ps.count)
       (begin matching ((subjects subjects) (ps ps))
         (match ps
           (()
            (eval-match subjects p-rest r))
           ((first @rest)
            (and (eval-match subjects.first first r)
                 (matching subjects.rest rest)))))))

(define (exp-vars-defined e)
  (match e
    ({constant _}  '())
    ({variable _}  '())
    ({make _ _ _}  '())
    ({do e1 e2}    (chain (exp-vars-defined e1)
                          (exp-vars-defined e2)))
    ({let p _}     (pat-vars-defined p))
    ({call e1 e2}  (chain (exp-vars-defined e1)
                          (exp-vars-defined e2)))
    ({list es}     (gather exp-vars-defined es))
    ({term tag e1} (exp-vars-defined e1))))

(define (pat-vars-defined p)
  (match p
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
    ({and-pat p1 p2}
     (chain (pat-vars-defined p1) (pat-vars-defined p2)))
    ({view-pat _ p1}
     (pat-vars-defined p1))))
