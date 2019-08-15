;; A kernel-Cant interpreter in direct-style Cant.
;; TODO is this still up to date wrt player.scm?

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
            (if (match message pattern pat-r)
                (play body (env-extend pat-r body-vars))
                (matching rest))))))))

(to (play e r)
  (may e.term
    (be {constant value}
      value)
    (be {variable name}
      (r name))
    (be {make _ stamp trait clauses}
      (actor<- (script<- (if trait (play trait r) miranda-trait)
                         clauses)
               r))
    (be {do e1 e2}                      ;eliminable (let _ e1)
      (play e1 r)
      (play e2 r))
    (be {let p e1}
      (let value (play e1 r))
      (if (match value p r)
          value
          (error "Match failure" p value)))
    (be {call e1 e2}
      (call (play e1 r) (play e2 r)))
    (be {term tag es}
      (term<- tag (for each ((e1 es))
                    (play e1 r))))
    (be {list es}
      (for each ((e1 es))
        (play e1 r)))))

(to (match subject p r)
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
      (and (match subject p1 r)
           (match subject p2 r)))
    (be {view-pat e p}
      (match ((play e r) subject) p r))))

(to (match-all values pats r)
  (hm (if values.none? pats.none?)
      (if pats.none? #no)
      (else (and (match     values.first pats.first r)
                 (match-all values.rest  pats.rest  r)))))


;; Environments

(make *uninitialized*)

(to (env-extend parent-r vars)
  (let values (array<-count vars.count *uninitialized*))
  (make env
    (to (_ key)
      (may (vars .find key #no)
        (be #no (parent-r key))
        (be i   (values i))))
    (to (_ .bind var value)
      (let i (vars .find var))
      (unless (= (values i) *uninitialized*)
        (error "Re-binding" var))
      (values .set! i value))))

(to (env<-map parent map)
  (make env
    (to (_ var)
      (take var (if (map .maps? var) map parent)))
    (to (_ .bind var value)
      (error "Tried to change immutable env" var))))

;; TODO interpose appropriate definitions for 'cant', 'use', 'load', ...
(make global-env
  (to (_ (? symbol? var))
    (cant .play var '()))             ;oof, expensive
  (to (_ .bind var val)
    (cant .play `(let ,var ',val) '())))


(export play env<-map env-extend global-env)
