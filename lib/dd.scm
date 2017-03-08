;; Decision diagrams
;; Choice nodes select a branch by the value of a variable, from 0 to
;; #branches-1. Each variable is also represented by a number, its 'rank'
;; -- lower rank numbers are placed nearer the root of the tree.

(import (use "lib/memoize") memoize)

(let infinity 999999)  ;; N.B. we don't have floats yet

(make none)  ;; Disjoint from all the values of constants.

(define (constant<- value)
  (surely (not= value none) "That can't be a constant value -- it's reserved")
  (make
    ({.rank}           infinity)
    ({.constant-value} value)
    ({.evaluate env}   value)
    ((@nodes)          (nodes value))))

(define (all-same? xs)
  (let set (call set<- xs))
  (= 1 set.count))

(define (choice<- rank branches)
  (surely (< rank infinity))
  (make choice
    ({.rank}           rank)
    ({.constant-value} none)
    ({.evaluate env}   ((branches (env rank)) .evaluate env))
    ({.branches}       branches)
    ((@nodes)          (case ((all-same? nodes)
                              nodes.first)
                             ((= (each '.constant-value nodes)
                                 (as-list nodes.keys))
                              choice)
                             (else
                              (memo-choice choice nodes))))))

(let memo-node (memoize choice<-))

(define (variable<- rank arity)
  (memo-node rank (each constant<- (range<- arity))))

(let memo-choice
  (memoize
   (given (node branches)
     (let top (arg-min `(,node ,@branches) '.rank))
     (let rank top.rank)
     (make-node rank
                (for each ((c top.branches.keys))
                  (call (subst rank c node) (subst-each rank c branches)))))))

(define (make-node rank branches)
  (if (all-same? branches)
      branches.first
      (memo-node rank branches)))

(define (subst-each rank value nodes)
  (for each ((e nodes)) (subst rank value e)))

(define (subst rank value node)
  (match (rank .compare node.rank)
    (-1 node) ; N.B. node must be a constant, iff we arrived here (XXX why?)
    ( 0 (node.branches value))
    (+1 (make-node node.rank
                   (subst-each rank value node.branches)))))

(define (valid? node)
  (not (satisfy node 0)))

(define (satisfy node goal)
  (let env (map<-))
  (begin walking ((node node))
    (if (not= none node.constant-value)
        (and (= goal node.constant-value)
             env)
        (for foldr/lazy (((value branch) node.branches.items)
                         (try-remaining-branches (given () #no)))
          (if (`(,none ,goal) .find? branch.constant-value)
              (do (env .set! node.rank value)
                  (walking branch))
              (try-remaining-branches))))))

(export constant<- variable<- satisfy valid?)
