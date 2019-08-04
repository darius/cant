;; Yet another compiler: lambda to VM.

(import (use 'ssets)  
  sset<- sset<-list sset-elements sset-remove sset-union sset-difference)

(import (use "eg/lambda/parser") exp-parse)
(import (use "eg/lambda/prelude") build-prelude)

(to (run lexp)
  (compile-and-run (exp-parse lexp)))

(to (compile-and-run exp)

  (to (fill e)
    (be e
      ({const c}        (const<- c))
      ({var v}          (var<- v))
      ({lam v body src} (lam<- v (fill body) src)) ;N.B. src isn't updated correspondingly
      ({app f a src}    (app<- (fill f) (fill a) src)) ;ditto
      ({the-env}        (fill exp))))  ; TODO this is pretty janky

  (to (apply fn arg)
    (invoke fn arg {halt}))

  (to (invoke fn arg k)
    (be fn
      ({compiled-closure entry _}
       (running entry {env fn arg} '() k))
      ({primitive p}
       (return (p arg) k))))

  (to (return result k)
    (be k
      ({frame pc r st ret-k}
       (running pc r (link result st) ret-k))
      ({halt}
       result)))

  (to (running pc r st k)  ; program counter + 1, "environment", local stack, "continuation"

    (to (fetch f)
      (let {env {compiled-closure _ vals} argument} r)
      (be f
        ('local     argument)
        ((? count?) (vals f))))

    ;;    (format "at ~w: ~w\n" pc (code (- pc 1)))
    (be (code (- pc 1))
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
    ((fill (exp-parse prelude)) .gen scope (prepend {return} nil-seq)))

  (let assembly (build-prelude interpret apply))
;;  (show assembly)

  (let code (array<-list (reverse assembly.insns)))  ; reverse so that labels index from the start
  (running code.count  ; program counter + 1
           {bad-env}   ; "environment"
           '()         ; local stack
           {halt}))    ; "continuation"


;; In the "assembly" language: The 'label' of an instruction is its
;; distance from the end of the list of all instructions. This is
;; easiest since we generate code back-to-front.
;; We cache the label in an object to keep constant-time access.

(make nil-seq
  ({.label} 0)
  ({.tail?} #no)
  ({.insns} '()))

(to (prepend insn rest)
  (let label (+ rest.label 1))
  (let insns (link insn rest.insns))
  (make link-seq
    ({.label} label)
    ({.tail?} (= insn {return}))
    ({.insns} insns)
    ({.rest}  rest)))                   ;kinda funky

;;XXX 
;;(to (show assembly)
;;  (for each! ((insn insns))
;;    (format "~2w ~w\n" insn.label insn)))


;; Expression objects cache the free-variable sets, again to avoid algorithmic blow-up.

(to (const<- c)
  (make constant
    ({.fvs} empty-set)
    ({.gen s then} (prepend {const c} then))))

(to (var<- v)
  (make variable
    ({.fvs} (sset<- v))
    ({.gen s then} (prepend (s v) then))))

(to (lam<- v body src)
  (let fvs (sset-remove body.fvs v))
  (make lambda
    ({.fvs} fvs)
    ({.gen s then}
     (let cvs ;; closure variables
       (sset-elements (sset-difference fvs (sset<-list s.known.keys))))
     (let fetches (for each ((v cvs))
                    (let {fetch f} (s v))
                    f))
     (prepend {enclose then.label fetches src}
              (body .gen
                    (scope<- v cvs.inverse s.known)
                    (prepend {return} then))))))

(to (app<- f a src)
  (make application
    ({.fvs} (sset-union f.fvs a.fvs))
    ({.gen s then}
     (let code (a .gen s
                  (f .gen s
                     (prepend {invoke src} (if then.tail? then.rest then)))))
     (if then.tail? code (prepend {push-cont then.label} code)))))

(let empty-set (sset<-))


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
         (be (var-offsets .get v)
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
