;; (Use run.scm to run this.)

(import (use 'memoize)
  memoize)

(let input (with-input-file _.read-all data-file))

(to (alt<- @es) {alt es})
(to (seq<- @es) {seq es})
(to (dir<- str) {dir str.first})

(let grammar (grammar<- "
main: '^' e '$'.
e:    f**'|' :Alt.
f:    g*     :Seq.
g:    dir | '(' e ')'.
dir:  {'N' | 'S' | 'E' | 'W'} :Dir.
"))
(let semantics (grammar (map<- `((Alt ,(feed alt<-))
                                 (Seq ,(feed seq<-))
                                 (Dir ,(feed dir<-))))))
(let parse-main (semantics 'main))
(to (parse string)
  (_.result (parson-parse parse-main string)))


(let infinity 999999)

(to (furthest-distance exp)
  ;; Map from each room position (relative to start) to the shortest
  ;; door-count distance found to it so far.
  (let rooms (map<-))

  ;; Maybe this would be simpler if we made the maze and then explored
  ;; it.  Maybe even more correct.  But let's try finishing this.
  (to (really-visit p d exp)
    ;; Return a map keyed by the positions you could end up by
    ;; following exp from p, with each value being the lowest
    ;; door-distance traveled to get to the position (including d).
    (rooms .set! p (min d (rooms .get p infinity)))
    (may exp
      ({dir ch}
       (visit (vector+ p (step ch))
              d.up
              {seq '()}))
      ({alt es}
       (merge-best (for each ((e es))
                     (visit p d e))))
      ({seq es}
       ;; Here's where we need the return value.
       (visit-seq p d es))))

  (to (really-visit-seq p d es)
    (if es.empty?
        (map<- `((,p ,d)))
        (do (let map1 (visit p d es.first))
            (merge-best (for each ((`(,p1 ,d1) map1.items))
                          (visit-seq p1 d1 es.rest))))))

  (let visit (memoize really-visit))
  (let visit-seq (memoize really-visit-seq))
  (visit '(0 0) 0 exp)
  (let how-many (for tally ((dist rooms.values))
                  (<= 1000 dist)))
  (format "count for >= 1000 distance: ~w\n" how-many)
  (max @rooms.values))

(to (vec? p)
  (and (list? p) (= p.count 2) (every number? p)))

(let empty-map (map<-))

(to (merge-best maps)
  (if maps.empty?
      empty-map
      (foldr1 merge2 maps)))

(to (merge2 map1 map2)
  ;; TODO there should be a union-with-combiner method
  (let result map1.copy)
  (for each! ((`(,k2 ,v2) map2.items))
    (result .set! k2 (may (map1 .get k2)
                       (#no v2)
                       (v1 (min v1 v2)))))
  result)

(let step (map<- '((#\N (0 -1))
                   (#\S (0  1))
                   (#\E (1  0))
                   (#\W (-1 0)))))

(to (test str)
  (format "~w...: " (str .slice 0 40))
  (let exp (parse str))
  (print (furthest-distance exp)))

(let eg1 "^WNE$")
(let eg2 "^ENWWW(NEEE|SSE(EE|N))$")
(let eg3 "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$")
(let eg4 "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$")
(let eg5 "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$")

(test eg1)
(test eg2)
(test eg3)
(test eg4)
(test eg5)

(to (part-1-and-2)
  (furthest-distance (parse input)))

(format "~w\n" (part-1-and-2))
