;; Decision diagrams

(define infinity 999999)  ;; N.B. we don't have floats yet

(define (constant<- value)
  (make
    ('rank ()           infinity)
    ('constant-value () value)
    ('evaluate (env)    value)
    ('choose (nodes)    (nodes value))))

(define (all-same? xs)
  ('= 1 ('count (set<-list xs))))

(define (set<-list xs)
  XXX)

(define (choice<- rank branches)
  (assert (< rank infinity))
  (make choice
    ('rank ()           rank)
    ('constant-value () #f)
    ('evaluate (env)    ('evaluate (branches (env rank)) env))
    ('branches ()       branches)
    ('choose (nodes)
      (if (all-same? nodes)
          (nodes 0)
          (if (equal? (map 'constant-value nodes) ;XXX equal?
                      (range<- ('count nodes)))
              choice
              (memo-choice choice nodes))))))

(define (memoize fn)
  ;; this is gonna need an eq?-hash table again...
  XXX)

(define memo-node (memoize choice<-))

(define (variable<- rank arity)
  (memo-node rank (map constant<- (range<- arity))))

(define (min-by xs key)
  XXX)

(define memo-choice
  (memoize
   (lambda (node branches)
     (let ((top (min-by (cons node branches) 'rank)))
       (let ((rank ('rank top)))
         (make-node rank
                    (map (lambda (c)
                           ('choose (subst rank c node)
                                    (map-subst rank c branches)))
                         (range<- ('count ('branches top))))))))))

(define (make-node rank branches)
  (if (all-same? branches)
      (branches 0)
      (memo-node rank branches)))

(define (map-subst rank value nodes)
  (map (lambda (e) (subst rank value e))
       nodes))

(define (subst rank value node)
  (if (< rank ('rank node))
      node ; N.B. node must be a constant, iff we arrived here (XXX why?)
      (if (= rank ('rank node))
          (('branches node) value)
          (make-node ('rank node)
                     (map-subst rank value ('branches node))))))

(define (valid? node)
  (is? #f (satisfy node 0)))

;; XXX ugly
(define (satisfy node goal)
  (letrec ((env (hashmap<-))             ;XXX
           (walking
            (lambda (node)
              (if ('constant-value node)
                  (if (is? goal ('constant-value node))
                      env
                      #f)
                  (trying ('rank node) 0 ('branches node)))))
           (trying
            (lambda (rank value branches)
              (if ('empty? branches)
                  #f
                  (if (memq? ('constant-value ('first branches))
                             (list<- #f goal))
                      (begin
                        ('set! env rank value)
                        (walking ('first branches)))
                      #f)))))
    (walking node)))
