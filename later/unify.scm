(let variable? symbol?)                 ;XXX not really

(define (variable<- prefix n)
  (symbol<- (.format (chain prefix ".%d") n)))

(make empty-subst
  (.subst (val)
    val)
  (.show ()
    '()))

(define (extend-unchecked s my-var my-val)
  (make extended-subst
    (.subst (val)
      (if (variable? val)
          (if (is? val my-var) my-val (.subst s val))
          val))
    (.show ()
      `((,my-var : ,my-val) ,@(.show s)))))

(define (extend s var val)
  (if (occurs? s var val) #f (extend-unchecked s var val)))

(define (occurs? s var val)
  (let val1 (.subst s val))
  (or (is? var val1)
      (and (list? val1)
           (for some ((item val1))
             (occurs? s var item)))))

(define (unify s val1 val2)
  (let u (.subst s val1))
  (let v (.subst s val2))
  (cond ((is? u v) s)
        ((variable? u)
         ((if (variable? v) extend-unchecked extend) s u v))
        ((variable? v)
         (extend s u v))
        ((and (list? u) (list? v) (= (.count u) (.count v)))
         (recurse unifying ((s s) (u u) (v v))
           (cond ((.empty? u) s)
                 (else
                  (let s1 (unify s (.first u) (.first v)))
                  (and s1 (unifying s1 (.rest u) (.rest v)))))))
        (else
         (and (equal? u v) s))))

(define (reify s val)
  (let free-vars (map<-))
  (recurse reifying ((val-in val))
    (let val (.subst s val))
    (cond ((variable? val)
           (unless (.has? free-vars val)
             (.set! free-vars val
                    (variable<- "_" (.count free-vars)))))
           (free-vars val))
          ((list? val)
           (each reifying val))
          (else
           val))))

;; TODO: consider making a 'failed' subst type instead of #f
;; or using 0-or-1-length lists. In fact, the latter meshes
;; perfectly with lazy-lists-as-Kanren-results.

