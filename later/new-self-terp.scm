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
    ({term tag es}
     (term<- tag (for each ((e1 es))
                   (eval e1 r))))
    ;; XXX make-trait?
    ))

(define (match subject p r)
  (case p
    ({match-any}
     #yes)
    ({match-var name}
     (env-resolve! r name subject)
     #yes)
    ({match-constant value}
     (= subject value))
    ({match-list ps}                    ;XXX what about @vars?
     (and (list? subject)
          (match-all subject ps r)))
    ({match-term tag ps}
     (and (term? subject)
          (match subject.tag tag r)     ;XXX make this always constant?
          (match-all subject.arguments ps r)))
    ({match-view e p1}
     (match (call (eval e r) subject)  ;;XXX (list<- subject)?
            p1 r))
    ;; I'm not sure we don't need more pattern types to be comfy
    ))

(define (match-all subjects ps r)
  (and (= subjects.count ps.count)
       (for every (((val p1) (zip subjects ps)))
         (match val p1 r))))

(define (exp-vars-defined e)
  (case e
    ({let p e1}       (pat-vars-defined p))
    ({sequence e1 e2} (chain (exp-vars-defined e1)
                             (exp-vars-defined e2)))
    (_                '())))

(define (pat-vars-defined p)
  (case p
    ({match-any}
     '())
    ({match-var name}
     `(,name))
    ({match-constant value}
     '())
    ({match-list ps}                    ;XXX what about @vars?
     (gather pat-vars-defined ps))
    ({match-term tag ps}
     (chain (pat-vars-defined tag)
            (gather pat-vars-defined ps)))
    ({match-view e p1}
     (pat-vars-defined p1))
    ))

;; TODO, probably: go back to allowing (let ...) anywhere, not only
;; right inside a (hide) or a (sequence)
