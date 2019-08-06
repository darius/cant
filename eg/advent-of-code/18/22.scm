;; (Use run.scm to run this.)
;; Simple least-cost search was too slow.

(import (use 'memoize)
  memoize)
(import (use 'pairing-heap)
  priority-queues<-)

(let input (with-input-file _.read-all data-file))

(let parse
  (simple-parser<- "'depth: ' :nat '\ntarget: ' [:nat ',' :nat :hug] '\n'"))

(let `(,depth ,target) (parse input))
(print depth)
(print target)

(let geologic-index<-
  (memoize (case
             ((= target) 0)
             ('(0 0)     0)
             (`(,x 0)    (* x 16807))
             (`(0 ,y)    (* y 48271))
             (`(,x ,y)   (* (erosion-level<- `(,(- x 1) ,y))
                            (erosion-level<- `(,x ,(- y 1))))))))

(to (erosion-level<- p)
  ((+ (geologic-index<- p) depth) .modulo 20183))

(to (type<- p)
  ((erosion-level<- p) .modulo 3))

(to (risk<- p)
 (type<- p))


(display "\nPart 1\n")

(to (part-1)
  (total-risk (rectangle<- '(0 0) target)))

(to (total-risk area)
  (sum (each risk<- area)))

(to (rectangle<- `(,xl ,yl) `(,xh ,yh))
  (grid* (xl .to xh)
         (yl .to yh)))

(format "~w\n" (part-1))


(display "\nPart 2\n")

(let usables (list<- (set<- 'climbing-gear 'torch)      ; rocky
                     (set<- 'climbing-gear 'neither)    ; wet
                     (set<- 'torch         'neither)))  ; narrow

(to (usable? tool p)
;;  (surely (symbol? tool) "tools are symbols")
;;  (surely (list? p) "coords")
  ((usables (type<- p)) .maps? tool))

(to (part-2)
  ;; A state is {state p tool} with position and current tool equipped.
  (let start-state {state '(0 0) 'torch})

  ;; Map from state to earliest time when it's been found to be
  ;; reachable, so far.
  (let bests (map<- `((,start-state 0))))

  ;; An 'effort' is a state at a time it can be reached. TODO better name?
  (let start-effort {at 0 start-state})

  (to (keep-early efforts)
    (for those (({at t state} efforts))
      (be (bests .get state)
        (#no (bests .set! state t)
             #yes)
        (t1  (if (< t t1)
                 (do (bests .set! state t)
                     #yes)
                 #no)))))

  (to (equip {at t {state p tool}} new-tool)
;;    (surely (usable? new-tool p) "New tool usable" new-tool)
    {at (+ t 7) {state p new-tool}})

  (to (time-bound<=? effort1 effort2)
    (<= (time-bound<- effort1)
        (time-bound<- effort2)))

  (to (time-bound<- {at t {state p tool}})
    (+ t (manhattan-distance p target) (if (= tool 'torch) 0 7)))

  (import (priority-queues<- time-bound<=?)
    pq-empty? pq-min
    empty-pq unit-pq pq-merge pq-insert pq-remove-min)

  (begin searching ((pq (pq-insert empty-pq start-effort)))
    (to (continue new-efforts)
      (searching (pq-merge (pq-remove-min pq)
                           (foldl pq-insert empty-pq (keep-early new-efforts)))))
    (let effort (pq-min pq))
    (let {at t state} effort)
    (let t0 (bests state))
    (if (< t0 t)
        (do (format "at ~3w skip\n" t)
            (continue '()))
        (do (let {state p tool} state)
            (format "at ~3w ~3w ~w size ~w\n"
                    t (manhattan-distance p target) state bests.count)
;;            (surely (usable? tool p) "Tool is still usable after move")
            (if (and (= p target) (= tool 'torch))
                t ; Final result: time to reach the target with torch equipped.
                (do (let moves
                      (for yeahs ((q (neighbors<- p)))
                        (and (usable? tool q)
                             {at t.up {state q tool}})))
                    (let swap (equip effort (swap-tool p tool)))
                    (continue `(,swap ,@moves))))))))

(to (swap-tool q tool)
  (let candidates ((usables (type<- q)) .keys))
  (if (= tool candidates.first)         ;TODO clumsy
      candidates.rest.first
      candidates.first))

(to (neighbors<- `(,x ,y))
  `((,x.up ,y)
    (,x ,y.up)
    ,@(if (< 0 x) `((,x.- ,y)) '())
    ,@(if (< 0 y) `((,x ,y.-)) '())))

;; (specialized from utils for speed)
(to (manhattan-distance p q)
  (+ (abs (- (p 0) (q 0)))
     (abs (- (p 1) (q 1)))))

(format "~w\n" (part-2))

;; Other answers: https://old.reddit.com/r/adventofcode/comments/a8i1cy/2018_day_22_solutions/
