;; from ~/git/mccarthy-to-bryant/lua/bdd3.lua

(let lit0 0)
(let lit1 1)

(define (constant<- value)
  (assert (or (= value lit0) (= value lit1))
          "Not binary" value)
  value)

(let infinite-rank #x7fffffff)
(let ranks (fillvector<- infinite-rank infinite-rank))
(let if0s  (fillvector<- lit0 lit1))
(let if1s  (fillvector<- lit0 lit1))
(let ifs   (vector<- if0s if1s))

(define (dedup memo k1 k2 k3)
  (define (enter map key)
    (let t (.get map key))
    (or t (hide (let v (map<-))
                (.set! map key v)
                v)))
  (let mem1 (enter memo k1))
  (let mem2 (enter mem1 k2))
  (list<- (.get mem2 k3) mem2))

(let choice-memo (map<-))

(define (build-choice rank if0 if1)
  (let (already memo-table) (dedup choice-memo rank if0 if1))
  (cond (already)
        (else
         (assert (< rank infinite-rank))
         (let index (.push ranks rank))
         (.push if0s if0)
         (.push if1s if1)
         (.set! memo-table if1 index)
         index)))

(define (make-choice rank if0 if1)
  (if (is? if0 if1)
      if0
      (build-choice rank if0 if1)))

(define (evaluate node env)             ;XXX name
  (cond ((<= node lit1) node)
        (else
         (let value (env (ranks node)))
         (evaluate ((ifs value) node) env))))

(define (do-choose node if0 if1)
  (cond ((<= node lit1)
         (case node
           (0 if0)                      ;N.B. 0 == lit0
           (1 if1)))
        ((is? if0 if1)
         if0)
        ((and (is? if0 lit0) (is? if1 lit1))
         node)
        (else
         (choose node if0 if1))))

(define (subst rank replacement node)
  (case (.compare rank (ranks node))
    (-1 node)
    ( 0 (do-choose replacement (if0s node) (if1s node)))
    (+1 (make-choice (ranks node)
                     (subst rank replacement (if0s node))
                     (subst rank replacement (if1s node))))))

(let choose-memo (map<-))

(define (choose node if0 if1)
  (let (already memo-table) (dedup choose-memo node if0 if1))
  (cond (already)
        (else
         (assert (< lit1 node))
         (let top (min (ranks node) (ranks if0) (ranks if1)))
         (let on0 (do-choose (subst top lit0 node)
                             (subst top lit0 if0)
                             (subst top lit0 if1)))
         (let on1 (do-choose (subst top lit1 node)
                             (subst top lit1 if0)
                             (subst top lit1 if1)))
         (let result (make-choice top on0 on1))
         (.set! memo-table if1 result)
         result)))

(define (satisfy-first node goal)
  (let goal-node (constant<- goal))
  (let env (map<-))
  (recurse walking ((node node))
    (cond ((<= node lit1)
           (and (is? node goal-node) env))
          (else
           (let if0 (if0s node))
           (cond ((or (< lit1 if0) (is? if0 goal-node))
                  (.set! env (ranks node) 0)
                  (walking if0))
                 (else
                  (.set! env (ranks node) 1)
                  (walking (if1s node))))))))
