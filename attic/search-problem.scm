;; Let's see what PAIP section 13.8 might look like.

"Protocol for a problem p:
p.states -> states
p .goal? state -> claim
p .successors state -> states
p .combine states states -> p
"

(to (main _)
  (eg1)
  (eg3))

(make fail)

(to (search p)
  (let states p.states)
  (if states.empty?
      fail
      (do (let state states.first)
          (if (p .goal? state)
              state
              (search (p .combine states.rest (p .successors state)))))))

(to (dfs-problem-maker<- prototype)     ;XXX better name for prototype?
  (to (dfs-problem<- states)
    (make dfs-problem
      ({.combine old new} (dfs-problem<- (chain new old)))
      ({.states} states)
      (message   (call prototype message)))))

(to (bfs-problem-maker<- prototype)
  (to (bfs-problem<- states)
    (make bfs-problem
      ({.combine old new} (bfs-problem<- (chain old new)))
      ({.states} states)                ;TODO factor out this duplication
      (message   (call prototype message)))))

(to (binary-tree-prototype<- goal)
  (make binary-tree-prototype
    ({.goal? state}
     (= state goal))
    ({.successors state}
     (let n (* 2 state))
     `(,n ,(+ n 1)))))

(to (eg1)
  (let maker (bfs-problem-maker<- (binary-tree-prototype<- 12)))
  (let p1 (maker '(1)))
  (print (search p1)))

(to (best-problem-maker<- prototype)
  (to (best-problem<- states)
    (make best-problem
      ({.combine old new} (best-problem<-
                           (sort-by (chain new old) prototype.cost-fn)))
      ({.states} states)
      (message   (call prototype message)))))

(to (eg3)
  (let goal 12)
  (let prototype (binary-tree-prototype<- goal))
  (let maker (best-problem-maker<-      ;XXX actually, best-beam-problem, in PAIP
              (make _
                ({.cost-fn} (given (state) (abs (- state goal))))
                (message (call prototype message)))))
  (let p3 (maker '(1)))
  ;; XXX best-beam-problem doesn't fit, because the .combine method in
  ;; best-problem calls best-problem<- itself, so we can't cleanly
  ;; delegate-then-modify the answer. I mean, we could go like
  ;; (beam-problem<- ((my-best-problem .combine old new) .states))
  ;; but that's kind of ugh. Maybe we should try and fix both this
  ;; problem and the above duplication to factor out, together.
  (print (search p3)))

(to (eg4)
  ;; TODO trip-problem
  )
