;; Let's try and make it easy to find instances of a pattern
;; in Squeam source code.

(to (main `(,_ ,@filenames))
  (each! search filenames))

(to (search filename)
  (when (find-it? (grab filename))
    (format "~d\n" filename)))

;; TODO fill in an actual pattern to search for
(to (find-it? e)

  (to (walk-exp exp)
    (match exp
      ({constant c} #no)
      ({variable v} #no)
      ({make name stamp-e extending-e clauses}
       (or (some walk-exp `(,stamp-e ,extending-e))
           (walk-clauses clauses)))
      ({do e1 e2}
       (or (walk-exp e1)
           (walk-exp e2)))
      ({let p e}
       (or (walk-pat p) (walk-exp e)))
      ({call e1 e2}
       (or (walk-exp e1) (walk-exp e2)))
      ({term tag es}
       (some walk-exp es))
      ({list es}
       (some walk-exp es))))

  (to (walk-clause `(,p ,p-vars ,e-vars ,e))
    (or (walk-pat p) (walk-exp e)))

  (to (walk-pat pat)
    (match pat
      ({any-pat} #no)
      ({variable-pat v} #no)
      ({constant-pat c} #no)
      ({view-pat e p}
       (or (walk-exp e) (walk-pat p)))
      ({and-pat p1 p2}
       (or (walk-pat p1) (walk-pat p2)))
      ({term-pat tag ps}
       (some walk-pat ps))))

  (walk-exp e))

;; XXX we need to parse incrementally, after
;;  pattern-matching on the raw sexprs
;; XXX duplicate code
(make grab
  (`(,filename)
   (grab filename '()))
  (`(,filename ,context)
   (let code (for with-input-file ((source filename))
               `(hide ,@(read-all source))))
   (parse-exp code context)))

(export search)
