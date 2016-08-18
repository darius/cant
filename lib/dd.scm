;; Decision diagrams
;; Choice nodes select a branch by the value of a variable, from 0 to
;; #branches-1. Each variable is also represented by a number, its 'rank'
;; -- lower rank numbers are placed nearer the root of the tree.
;; XXX untested

(import (use "lib/memoize.scm") memoize)

(let infinity 999999)  ;; N.B. we don't have floats yet

(define (constant<- value)
  (assert value "You can't use #no as a constant value -- it's reserved") ;XXX well, fix it
  (make
    ({.rank}           infinity)
    ({.constant-value} value)
    ({.evaluate env}   value)
    ({.choose nodes}   (nodes value))))

(define (all-same? xs)
  (let set (call set<- xs))
  (= 1 set.count))

(define (choice<- rank branches)
  (assert (< rank infinity))
  (make choice
    ({.rank}           rank)
    ({.constant-value} #no) ; The reason for the above reserving of #no.
    ({.evaluate env}   ((branches (env rank)) .evaluate env))
    ({.branches}       branches)
    ({.choose nodes}   (case ((all-same? nodes)
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
                  ((subst rank c node) .choose (subst-each rank c branches)))))))

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
    (if node.constant-value
        (and (= goal node.constant-value)
             env)
        (for foldr/lazy (((value branch) node.branches.items)
                         (try-remaining-branches (given () #no)))
          (if (`(#no ,goal) .maps-to? branch.constant-value)
              (do (env .set! node.rank value)
                  (walking branch))
              (try-remaining-branches))))))
