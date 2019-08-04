;; A bit of Forth with local variables

(to (stack-op-2-1<- op)
  (to (stack-op-2-1 {state `(,z ,y ,@s) r})
    {state `(,(op y z) ,@s) r}))

(to (push<- literal)
  (to (push {state s r})
    {state `(,literal ,@s) r}))

(to (grab<- count)
  (to (grab {state s r})
    {state (s .slice count)
           (chain (s .slice 0 count) r)}))

(to (local<- k)
  (to (local {state s r})
    {state `(,(r k) ,@s) r}))

(to (ungrab<- count)
  (to (ungrab {state s r})
    {state s (r .slice count)}))

(let dictionary
  (map<- `((+ ,(stack-op-2-1<- +))
           (- ,(stack-op-2-1<- -))
           (* ,(stack-op-2-1<- *))
           (/ ,(stack-op-2-1<- /))
           )))

(to (compile tokens)
  (let code (flexarray<-))
  (begin compiling ((frames '()) (tokens tokens))
    (case (tokens.empty?
           (surely frames.empty?))      ;XXX require
          (else
           (match tokens.first
             ((? number? n)
              (code .push! (push<- n))
              (compiling frames tokens.rest))
             ('<<
              (let `(,locals ,tail)
                (split-on (-> (= it '--)) tokens.rest))
              (code .push! (grab<- locals.count))
              (compiling `(,(reverse locals) ,@frames)
                         tail.rest))
             ('>>
              (surely (not frames.empty?)) ;XXX require
              (code .push! (ungrab<- frames.first.count))
              (compiling frames.rest tokens.rest))
             ((? symbol? word)
              (code .push! (or (compile-local frames word)
                               (dictionary word)))
              (compiling frames tokens.rest))))))
  code.values)

(to (compile-local frames word)
  (let locals (call chain frames))      ;I think
  (match (locals .find word #no)
    (#no   #no)
    (index (local<- index))))

(to (run xts)
  (for foldl ((state {state '() '()})
              (xt xts))
    (xt state)))

(let eg1 '(4 3 * 2 /))
(let eg2 '(5 3 << a b -- a a * b - >>))

(to (main _)
  (let {state s1 _} (run (compile eg1)))
  (print s1)
  (let {state s2 _} (run (compile eg2)))
  (print s2)
  )

(export main)
