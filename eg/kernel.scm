;; A kernel-Squeam interpreter in direct-style Squeam

;; Variable names:
;;   e expression
;;   p pattern
;;   r environment

(to (actor<- script r)
  (make actor
    (to message
      (script .receive message actor r))))

(to (script<- trait clauses)
  (make script
    (to (_ .receive message actor parent-r)
      (begin matching ((clauses clauses))
        (be clauses
          ('()
           (trait actor message))
          (`((,pattern ,pat-vars ,body-vars ,body) ,@rest)
           (let pat-r (env-extend parent-r pat-vars))
           (if (eval-match message pattern pat-r)
               (eval body (env-extend pat-r body-vars))
               (matching rest))))))))

(to (eval e r)
  (be e.term
    ({constant value}
     value)
    ({variable name}
     (r name))
    ({make _ stamp trait clauses}
     (actor<- (script<- (if trait (eval trait r) miranda-trait)
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
    ({term tag es}
     (term<- tag (for each ((e1 es))
                   (eval e1 r))))
    ({list es}
     (for each ((e1 es))
       (eval e1 r)))))

(to (eval-match subject p r)
  (be p.term
    ({any-pat}
     #yes)
    ({variable-pat name}
     (r .bind name subject)
     #yes)
    ({constant-pat value}
     (= subject value))
    ({list-pat p-args}
     (and (list? subject)
          (match-all subject p-args r)))
    ({term-pat tag p-args}
     (and (term? subject)
          (= subject.tag tag) ;; N.B. it'd be nice for efficiency to bundle the arity in at desugar time
          (match-all subject.arguments p-args r)))
    ({and-pat p1 p2}
     (and (eval-match subject p1 r)
          (eval-match subject p2 r)))
    ({view-pat e p}
     (eval-match (call (eval e r) `(,subject))
                 p r))))

(to (match-all values pats r)
  (hm (if values.empty? pats.empty?)
      (if pats.empty? #no)
      (else (and (eval-match values.first pats.first r)
                 (match-all  values.rest  pats.rest  r)))))


;; Environments

(make *uninitialized*)

(to (env-extend parent-r vars)
  (let vals (array<-count vars.count *uninitialized*))
  (make env
    (to `(,key)
      (be (vars .find key #no)
        (#no (parent-r key))
        (i   (vals i))))
    (to (_ .bind var val)
      (let i (vars .find var))
      (unless (= (vals i) *uninitialized*)
        (error "Re-binding" var))
      (vals .set! i val))))

(to (env<-map map)
  (make env
    (to `(,key)
      (map key))
    (to (_ .bind var val)
      (error "Tried to change immutable env" var))))
  
(let global-env
  (env<-map (export __as-link + - * /)))   ; etc.

(export eval env<-map env-extend global-env)
