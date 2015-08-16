;; Decision diagrams

(let infinity 999999)  ;; N.B. we don't have floats yet

(define (constant<- value)
  (make
    (.rank ()           infinity)
    (.constant-value () value)
    (.evaluate (env)    value)
    (.choose (nodes)    (nodes value))))

(define (all-same? xs)
  (= 1 (.count (set<-list xs))))

(define (choice<- rank branches)
  (assert (< rank infinity))
  (make choice
    (.rank ()           rank)
    (.constant-value () #no)
    (.evaluate (env)    (.evaluate (branches (env rank)) env))
    (.branches ()       branches)
    (.choose (nodes)
      (cond ((all-same? nodes)
             (nodes 0))
            ((equal? (each '.constant-value nodes)
                     (range<- (.count nodes)))
             choice)
            (else
             (memo-choice choice nodes))))))

(let memo-node (memoize choice<-))

(define (variable<- rank arity)
  (memo-node rank (each constant<- (range<- arity))))

(let memo-choice
  (memoize
   (given (node branches)
     (let top (minimum-by (cons node branches) '.rank))
     (let rank (.rank top))
     (make-node rank
                (for each ((c (range<- (.count (.branches top)))))
                  (.choose (subst rank c node)
                           (subst-each rank c branches)))))))

(define (make-node rank branches)
  (if (all-same? branches)
      (branches 0)
      (memo-node rank branches)))

(define (subst-each rank value nodes)
  (for each ((e nodes)) (subst rank value e)))

(define (subst rank value node)
  (cond ((< rank (.rank node))
         node) ; N.B. node must be a constant, iff we arrived here (XXX why?)
        ((= rank (.rank node))
         ((.branches node) value))
        (else
         (make-node (.rank node)
                    (subst-each rank value (.branches node))))))

(define (valid? node)
  (not (satisfy node 0)))

;; XXX ugly
(define (satisfy node goal)
  (let env (map<-))
  (recurse walking ((node node))
    (if (.constant-value node)
        (and (is? goal (.constant-value node))
             env)
        (recurse trying ((rank (.rank node))
                         (value 0)
                         (branches (.branches node)))
          (and (not (.empty? branches))
               (cond ((memq? (.constant-value (.first branches))
                             (list<- #no goal))
                      (.set! env rank value)
                      (walking (.first branches)))
                     (else
                      (trying rank (+ value 1) (.rest branches)))))))))
