;; Binary decision diagrams
;; from ~/git/mccarthy-to-bryant/lua/bdd3.lua

(to (bdd-and f g) (do-choose g lit0 f)) ;TODO rename do-choose
(to (bdd-or  f g) (do-choose g f lit1))

(let lit0 0)
(let lit1 1)

(to (constant<- value)
  (surely (or (= value lit0) (= value lit1))
          "Not binary" value)
  value)

(let infinite-rank 0x7fffffff)
(let ranks (flexarray<- infinite-rank infinite-rank))
(let if0s  (flexarray<- lit0 lit1))
(let if1s  (flexarray<- lit0 lit1))
(let ifs   [if0s if1s])

(to (dedup memo k1 k2 k3)
  (let mem1 (memo .get-set! k1 map<-))
  (let mem2 (mem1 .get-set! k2 map<-))
  (list<- (mem2 .get k3) mem2))

(let choice-memo (map<-))

(to (build-choice rank if0 if1)
  (let `(,already ,memo-table) (dedup choice-memo rank if0 if1))
  (or already
      (do (surely (< rank infinite-rank))
          (let index (ranks .push! rank))
          (if0s .push! if0)
          (if1s .push! if1)
          (memo-table .set! if1 index)
          index)))

(to (make-choice rank if0 if1)
  (if (= if0 if1)
      if0
      (build-choice rank if0 if1)))

(to (bdd-evaluate node env)
  (if (<= node lit1)
      node
      (do (let value (env (ranks node)))
          (bdd-evaluate ((ifs value) node) env))))

(to (do-choose node if0 if1)
  (hm (if (<= node lit1)
          (be node
            (0 if0)                      ;N.B. 0 == lit0
            (1 if1)))
      (if (= if0 if1)
          if0)
      (if (and (= if0 lit0) (= if1 lit1))
          node)
      (else
          (choose node if0 if1))))

(to (subst rank replacement node)
  (be (rank .compare (ranks node))
    (-1 node)
    ( 0 (do-choose replacement (if0s node) (if1s node)))
    (+1 (make-choice (ranks node)
                     (subst rank replacement (if0s node))
                     (subst rank replacement (if1s node))))))

(let choose-memo (map<-))

(to (choose node if0 if1)
  (let `(,already ,memo-table) (dedup choose-memo node if0 if1))
  (or already
      (do (surely (< lit1 node))
          (let top (min (ranks node) (ranks if0) (ranks if1)))
          (let on0 (do-choose (subst top lit0 node)
                              (subst top lit0 if0)
                              (subst top lit0 if1)))
          (let on1 (do-choose (subst top lit1 node)
                              (subst top lit1 if0)
                              (subst top lit1 if1)))
          (let result (make-choice top on0 on1))
          (memo-table .set! if1 result)
          result)))

;; Return the lexicographically first env which makes node evaluate to
;; goal, if possible; else #no.
(to (satisfy-first node goal)
  (let goal-node (constant<- goal))
  (let env (map<-))
  (begin walking ((node node))
    (hm (when (<= node lit1)
          (and (= node goal-node) env))
        (do (let if0 (if0s node)))
        (when (or (< lit1 if0) (= if0 goal-node))
          (env .set! (ranks node) 0)
          (walking if0))
        (else
          (env .set! (ranks node) 1)
          (walking (if1s node))))))

(export satisfy-first build-choice bdd-and bdd-or lit0 lit1 constant<- bdd-evaluate)
;; TODO export more, rename
