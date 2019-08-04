;; (Use run.scm to run this.)
;; TODO: keep cleaning this up

(let `(,n-workers ,min-delay)
;  '(2 1))
;  '(2 61))
  '(5 61))
(let input (with-input-file '.read-lines data-file))

(let parse
  (simple-parser<- "'Step ' :anyone ' must be finished before step ' :anyone ' can begin.'"))

(let ordering (each parse input))
;(each! print ordering)

(let all-steps (set<-list (call chain ordering))) ;TODO use list.range method now
(let nodes all-steps.keys)
(let succs-set (for map-by ((_ nodes))
                 (set<-)))
(for each! ((`(,pre ,post) ordering))   ;TODO use a mapreduce
  ((succs-set pre) .add! post))             ;TODO maybe use .get-set!

(let succs (for map-by ((node nodes))
             (sort ('.keys (succs-set node)))))

(to (merge xs ys)                       ;TODO should be in a sort module or something
  (foldr insert ys xs))

(to (insert x xs)                       ;TODO should be in a heapq or sort module or something
  (hm (if (or xs.empty? (< x xs.first))
          `(,x ,@xs))
      (if (= x xs.first)
          xs)
      (else
         `(,xs.first ,@(insert x xs.rest)))))

(to (delay<- node)                ;maybe go back to calling them steps
  (let letter (node 0))            ;symbols aren't looking so great now
  (+ min-delay (- letter #\A)))

;; I think we could've kept more of the original structure.
;; semischedule: retirement stream -> available-jobs-list stream
;; then fill workers from available jobs, and send retirement notices

(to (schedule)
  (let pred-count (for map-by ((_ nodes))
                    0))
  (for each! ((ss succs.values))
    (for each! ((s ss))
      (pred-count .set! s (+ (pred-count s) 1))))   ;TODO maybe incr a box instead

  (let outbox (flexarray<-))

  (to (assign arg)
    ;; TODO can be simpler for initial assignment
;    (format "assign ~w\n" arg)
    (let {state workers jobs} arg)
    (begin assigning ((ws1 workers) (ws2 '()) (jobs jobs))
;      (format "ws1 = ~w\n" ws1)
      (be ws1
        ('()
         {state (reverse ws2) jobs})
        (`(#no ,@rest)
         (be jobs
           ('()
            (assigning rest `(#no ,@ws2) '()))
           (`(,j ,@js)
            (let delay (delay<- j))
            (assigning rest `((,j ,delay) ,@ws2) js))))
        (`(,w ,@rest)
         (assigning rest `(,w ,@ws2) jobs)))))

  (to (work {state workers jobs})
    ;; Decrement one time tick on each worker,
    ;; and retire the nodes that reach 0.
    (begin working ((ws1 workers) (ws2 '()) (jobs jobs))
;      (format "a: ws1 = ~w\n" ws1)
      (be ws1
        ('()
;         (print 'b)
         {state (reverse ws2) jobs})
        (`(#no ,@rest)
         (working rest `(#no ,@ws2) jobs))
        (`((,node ,ticks-left) ,@rest)
;         (print 'c)
         (let n (- ticks-left 1))
         (if (= n 0)
             (do (outbox .push! node)
                 (let new-nodes
                   (sort (for those ((s (succs node)))
                           (surely (< 0 (pred-count s)))
                           (pred-count .set! s (- (pred-count s) 1)) ;TODO box
                           (= 0 (pred-count s)))))
                 ;; let's assume assigning happens elsewhere for now
                 (working rest `(#no ,@ws2) (merge new-nodes jobs)))
             (working rest `((,node ,n) ,@ws2) jobs))))))

  (let first-steps (sort (for those ((node nodes))
                           (= 0 (pred-count node)))))

  (format "Second Workers Pending Done\n")
  (begin ticking ((t 0)
                  ({state workers jobs}
                   (assign {state ('(#no) .repeat n-workers)
                                  first-steps})))
    (format "~6w ~7w ~7w ~w\n"
            t
            (for each ((w workers))
              (if w (w 0) '-))
            jobs
            ("" .join outbox.values))
    (if (some identity workers) ;clumsy, I guess
        (ticking (+ t 1) (assign (work {state workers jobs})))
        t)))


(display "Part 2\n")

(let total-delay (schedule))
(format "\nresult 2: ~w\n" total-delay)
