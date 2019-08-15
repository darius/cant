;; Let's try and make it easy to find instances of a pattern
;; in Cant source code.
;;XXX untested - e.g. walk-clauses below isn't filled in

(to (main `(,_ ,@filenames))
  (each! search filenames))

(to (search filename)
  (when (find-it? (grab filename))
    (format "~d\n" filename)))

;; TODO fill in an actual pattern to search for
(to (find-it? e)

  (to (walk-exp exp)
    (may exp
      (be {constant c} #no)
      (be {variable v} #no)
      (be {make name stamp-e extending-e clauses}
        (or (some walk-exp `(,stamp-e ,extending-e))
            (walk-clauses clauses)))
      (be {do e1 e2}
        (or (walk-exp e1)
            (walk-exp e2)))
      (be {let p e}
        (or (walk-pat p) (walk-exp e)))
      (be {call e1 e2}
        (or (walk-exp e1) (walk-exp e2)))
      (be {term tag es}
        (some walk-exp es))
      (be {list es}
        (some walk-exp es))))

  (to (walk-clause `(,p ,p-vars ,e-vars ,e))
    (or (walk-pat p) (walk-exp e)))

  (to (walk-pat pat)
    (may pat
      (be {any-pat} #no)
      (be {variable-pat v} #no)
      (be {constant-pat c} #no)
      (be {view-pat e p}
        (or (walk-exp e) (walk-pat p)))
      (be {and-pat p1 p2}
        (or (walk-pat p1) (walk-pat p2)))
      (be {term-pat tag ps}
        (some walk-pat ps))))

  (walk-exp e))

;; XXX we need to parse incrementally, after
;;  pattern-matching on the raw sexprs
;; XXX duplicate code
(make grab
  (to (_ filename)
    (grab filename '()))
  (to (_ filename ,context)
    (let code (for with-input-file ((source filename))
                `(hide ,@(read-all source))))
    (cant .parse-expression code context)))

(export search)
