;; A kernel-Squeam interpreter in direct-style Squeam
;; TODO is this still up to date wrt terp.scm?

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
        (may clauses
          (be '()
            (trait actor message))
          (be `((,pattern ,pat-vars ,body-vars ,body) ,@rest)
            (let pat-r (env-extend parent-r pat-vars))
            (if (eval-match message pattern pat-r)
                (eval body (env-extend pat-r body-vars))
                (matching rest))))))))

(to (eval e r)
  (may e.term
    (be {constant value}
      value)
    (be {variable name}
      (r name))
    (be {make _ stamp trait clauses}
      (actor<- (script<- (if trait (eval trait r) miranda-trait)
                         clauses)
               r))
    (be {do e1 e2}
      (eval e1 r)
      (eval e2 r))
    (be {let p e1}
      (let value (eval e1 r))
      (if (eval-match value p r)
          value
          (error "Match failure" p value)))
    (be {call e1 e2}
      (call (eval e1 r) (eval e2 r)))
    (be {term tag es}
      (term<- tag (for each ((e1 es))
                    (eval e1 r))))
    (be {list es}
      (for each ((e1 es))
        (eval e1 r)))))

(to (eval-match subject p r)
  (may p.term
    (be {any-pat}
      #yes)
    (be {variable-pat name}
      (r .bind name subject)
      #yes)
    (be {constant-pat value}
      (= subject value))
    (be {list-pat p-args}
      (and (list? subject)
           (match-all subject p-args r)))
    (be {term-pat tag p-args}
      (and (term? subject)
           (= subject.tag tag) ;; N.B. it'd be nice for efficiency to bundle the arity in at desugar time
           (match-all subject.arguments p-args r)))
    (be {and-pat p1 p2}
      (and (eval-match subject p1 r)
           (eval-match subject p2 r)))
    (be {view-pat e p}
      (eval-match ((eval e r) subject) p r))))

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
    (to (_ key)
      (may (vars .find key #no)
        (be #no (parent-r key))
        (be i   (vals i))))
    (to (_ .bind var val)
      (let i (vars .find var))
      (unless (= (vals i) *uninitialized*)
        (error "Re-binding" var))
      (vals .set! i val))))

(to (env<-map map)
  (make env
    (to (_ key)
      (map key))
    (to (_ .bind var val)
      (error "Tried to change immutable env" var))))
  
(let global-env
  (env<-map (export __as-link + - * /)))   ; etc.

(export eval env<-map env-extend global-env)
