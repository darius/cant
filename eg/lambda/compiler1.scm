;; Yet another compiler: lambda to VM.

(import (use "eg/lambda/parser") exp-parse)
(import (use "eg/lambda/prelude") build-prelude)

(to (run lexp)
  (compile-and-run (exp-parse lexp)))

(to (compile-and-run exp)

  (to (fill e)
    (match e
      ({const c}        (const<- c))
      ({var v}          (var<- v))
      ({lam v body src} (lam<- v (fill body) src)) ;N.B. src isn't updated correspondingly
      ({app f a src}    (app<- (fill f) (fill a) src)) ;ditto
      ({the-env}        (fill exp))))  ; TODO this is pretty janky

  (to (apply fn arg)
    (invoke fn arg {halt}))

  (to (invoke fn arg k)
    (match fn
      ({compiled-closure entry _}
       (running entry {env fn arg} '() k))
      ({primitive p}
       (return (p arg) k))))

  (to (return result k)
    (match k
      ({frame pc r st ret-k}
       (running pc r (link result st) ret-k))
      ({halt}
       result)))

  (to (running pc r st k)  ; program counter + 1, "environment", local stack, "continuation"

    (to (fetch f)
      (let {env {compiled-closure _ vals} argument} r)
      (match f
        ('local     argument)
        ((? count?) (vals f))))

    ;;    (format "at ~w: ~w\n" pc (code (- pc 1)))
    (match (code (- pc 1))
      ({const c}
       (running (- pc 1) r (link c st) k))
      ({fetch f}
       (running (- pc 1) r (link (fetch f) st) k))
      ({enclose addr fs _}
       (let closure {compiled-closure (- pc 1) (each fetch fs)}) ;TODO link to src annotation somewhere
       (running addr r (link closure st) k))
      ({push-cont addr}
       (running (- pc 1) r st {frame addr r st k}))
      ({return}
       (return st.first k))
      ({invoke _}
       (let `(,fn ,arg ,@_) st)
       (invoke fn arg k))
      ))

  (to (interpret prelude {module builtins}) ;TODO rename
    (let scope (module-scope<- (map<- (for each ((`(,k ,v) builtins.items))
                                        `(,k ,{const v})))))
    ((fill (exp-parse prelude)) .gen scope '({return})))

  (let the-insns (build-prelude interpret apply))
;;  (show the-insns)

  (let code (array<-list (reverse the-insns)))  ; reverse so that labels index from the start
  (running code.count  ; program counter + 1
           {bad-env}   ; "environment"
           '()         ; local stack
           {halt}))    ; "continuation"


;; In the "assembly" language: The 'label' of an instruction is its
;; distance from the end of the list of all instructions. This is
;; easiest since we generate code back-to-front.
;; TODO: well, that's simple but it's O(n) time

(to (const<- c)
  (make constant
    ({.fvs} empty-set)
    ({.gen s then} (link {const c} then))))

(to (var<- v)
  (make variable
    ({.fvs} (set<- v))
    ({.gen s then} (link (s v) then))))

(to (lam<- v body src)
  (let fvs (body.fvs .difference (set<- v)))
  (make lambda
    ({.fvs} fvs)
    ({.gen s then}
     (let cvs ;; closure variables
       (sort ((fvs .difference (set<-list s.known.keys)) .keys)))
     (let fetches (for each ((v cvs))
                    (let {fetch f} (s v))
                    f))
     (link {enclose then.count fetches src}
           (body .gen
                 (scope<- v cvs.inverse s.known)
                 (link {return} then))))))

(to (app<- f a src)
  (make application
    ({.fvs} (f.fvs .union a.fvs))
    ({.gen s then}
     (let tail? (= then.first {return}))
     (let code (a .gen s
                  (f .gen s
                     (link {invoke src} (if tail? then.rest then)))))
     (if tail? code (link {push-cont then.count} code)))))

(to (show insns)
  (for each! ((`(,i ,insn) insns.items))
    (format "~2w ~w\n" (- insns.count i) insn)))

(let empty-set (set<-))


;; Scopes (called 's' above)

(to (module-scope<- known)
  (make module-scope
    (`(,v) (known v))
    ({.known} known)))

(to (scope<- param var-offsets known)
  (make scope
    (`(,v)
     (if (= v param)
         {fetch 'local}
         (match (var-offsets .get v)
           (#no (known v))
           (n {fetch n}))))
    ({.known} known)))


;; Smoke test

(let eg '(([x y] x) ([z] z)))
(print eg)
(print (run eg))
(newline)

(let eg2 '(([x] (add1 x)) 5))
(print (run eg2))


;; Main

(to (main argv)
  (for each! ((filename argv.rest))
    (format "\n~d:\n" filename)
    (print (run-file filename))))

(to (run-file filename)
  (let es (with-input-file read-all filename))
  (run `(do ,@es)))

(export run run-file)
