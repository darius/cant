;; Prolog-style unification
;; XXX needs a lot of work.

;; TODO: work on terms, not just lists

(let variable? array?) ;TODO make a disjoint type instead with stamps

(to (variable<- prefix n)
  [(symbol<- (chain prefix "." (string<-number n)))])

(to (variable-name var)
  (var 0))

;; XXX seems clumsy:
(to (apply s val)                   ;XXX rename
  (let v (s .subst val))
  (if (variable? v)
      (if (= v val) v (apply s v))
      v))

(make empty-subst
  (to (_ .subst val)
    val)
  (to (_ .selfie sink)
    (sink .display "<>")))

(to (extend-unchecked s my-var my-val)
  (make extended-subst
    (to (_ .subst val)
      (if (variable? val)
          (if (= val my-var) my-val (s .subst val))
          val))
    (to (_ .selfie sink)
      (format .to-sink sink "<~d: ~w>..~w" (variable-name my-var) my-val s))))

(to (extend s var val)
  (if (occurs? s var val) #no (extend-unchecked s var val)))

(to (occurs? s var val)
  (let val1 (s .subst val))
  (or (= var val1)
      (and (list? val1)
           (for some ((item val1))
             (occurs? s var item)))))

(to (unify s val1 val2)
  (let u (s .subst val1))
  (let v (s .subst val2))
  (hm (if (variable? u)
          (if (= u v)
              s
              ((if (variable? v) extend-unchecked extend) s u v)))
      (if (variable? v)
          (extend s u v))
      (if (and (list? u) (list? v) (= u.count v.count))
          (begin unifying ((s s) (u u) (v v))
            (if u.empty?
                s
                (do (let s1 (unify s u.first v.first))
                    (and s1 (unifying s1 u.rest v.rest))))))
      (else
          (and (= u v) s))))

(to (reify s val)
  (let free-vars (map<-))
  (begin reifying ((val-in val))
    (let val (apply s val-in))
    (hm (when (variable? val)
          (unless (free-vars .maps? val)
            (free-vars .set! val
                       (variable<- "_" free-vars.count)))
          (free-vars val))
        (when (list? val)
          (each reifying val))
        (else
          val))))

;; TODO: consider making a 'failed' subst type instead of #no
;; or using 0-or-1-length lists. In fact, the latter meshes
;; perfectly with lazy-lists-as-Kanren-results.

(export variable? variable<- unify empty-subst reify)
