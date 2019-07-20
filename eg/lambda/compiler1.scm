;; Yet another compiler: lambda to VM.

(import (use "eg/lambda/parser") exp-parse)

(to (run lexp)
  (run-code (compile lexp)))

(to (compile e)
  (codegen global-scope (exp-parse e) '({halt})))

;; In the "assembly" language: The 'label' of an instruction is its
;; distance from the end of the list of all instructions. This is
;; easiest since we generate code back-to-front.

(to (free-vars<- e)
  (match e
    ({const c}      empty-set)
    ({var v}        (set<- v))
    ({lam v body _} ((free-vars<- body) .difference (set<- v)))
    ({app f a _}    ((free-vars<- f) .union (free-vars<- a)))))

(let empty-set (set<-))

(to (codegen s e then)
  (match e
    ({const c}
     (link {const c} then))
    ({var v}
     (link {fetch (s v)} then))
    ({lam v body src}
     (let fv (free-vars<- e))
     (link {enclose then.count (each s fv.keys) src}
           (codegen (scope<- v fv.keys.inverse) body
                    (link {return} then))))
    ({app f a src}
     (let tail? (= then.first {return}))
     (let code (codegen s a
                        (codegen s f
                                 (link {invoke src} (if tail? then.rest then)))))
     (if tail? code (link {push-cont then.count} code)))))


;; Scopes (called 's' above)

(to (global-scope v)
  (error "Unbound variable" v))

(to ((scope<- param var-offsets) v)
  (if (= v param)
      'local
      (var-offsets v)))


;; VM

(to (show code)
  (for each! ((`(,i ,insn) code.items))
    (format "~2w ~w\n" (- code.count i) insn)))

(to (run-code insns)
  (let code (array<-list (reverse insns)))  ; reverse so that labels index from the start
  (begin running ((pc code.count)  ; program counter + 1
                  (r {bad-env})    ; "environment"
                  (st '())         ; local stack
                  (k {bad-frame})) ; "continuation"
    (to (fetch f)
      (let {env {compiled-closure _ vals} argument} r)
      (match f
        ('local     argument)
        ((? count?) (vals f))))
    (to (restack stack)
      (running (- pc 1) r stack k))
    (to (return result)
      (let {frame ret-pc ret-r ret-st ret-k} k)
      (running ret-pc ret-r (link result ret-st) ret-k))

;;    (format "at ~w: ~w\n" pc (code (- pc 1)))
    (match (code (- pc 1))
      ({halt}
       (surely (= st.count 1))
       st.first)
      ({const c}
       (restack (link c st)))
      ({fetch f}
       (restack (link (fetch f) st)))
      ({enclose addr fs _}
       (let closure {compiled-closure (- pc 1) (each fetch fs)}) ;TODO link to src annotation somewhere
       (running addr r (link closure st) k))
      ({push-cont addr}
       (running (- pc 1) r st {frame addr r st k}))
      ({return}
       (return st.first))
      ({invoke _}
       (let `(,fn ,arg ,@_) st)
       (match fn
         ({compiled-closure entry _}
          (running entry {env fn arg} '() k))
         ({primitive p}
          (return (p arg)))))
      )))


;; Smoke test

(let eg '(([x y] x) ([z] z)))
(print eg)
(show (compile eg))

(print (run eg))
