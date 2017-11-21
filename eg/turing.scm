;; Turing machine interpreter
;; XXX untested

;; mark = value that can appear in a tape square.
;; state = function from mark to `(,actions ,state-id).
;; action = '< | '> | mark
;;   (< = step left, > = right)

(to (turer<- states start-state-id)
  (let state-id (box<- start-state-id))
  (let tape (box<- '(() - ()))) ;; `(,ls ,head ,rs) where:
  ;; ls   = reverse of the list of tape squares to the left of the tape head.
  ;; head = the value of the square under the tape head.
  ;; rs   = the squares to the right of the tape head.
  ;; The lists ls and rs only extend so far as has been visited up to now.
  ;; Visited blank squares are symbolized as '-'.

  (make turer

    ({reset! marks}
     (let `(,h ,@R) (peek marks))
     (tape .^= `(() ,h ,R))
     (state-id .^= start-state-id))

    ({run} (turer .run #no))
    ({run option}
     (begin running ()
       (match option
         ('loudly (print turer.show-config)))
       (when (states .maps? state-id.^)
         turer.step
         (running))))

    ({step}
     (let `(,_ ,h ,_) tape.^)
     (let `(,acts ,next-state-id) ((states state-id.^) h))
     (tape .^= (for foldl ((t tape.^) (act acts))
                 (let `(,L ,h ,R) t)
                 (match act
                   ('< (let `(,l0 ,@ls) (peek L))
                       `(,ls ,l0 (,h ,@R)))
                   ('> (let `(,r0 ,@rs) (peek R))
                       `((,h ,@L) ,r0 ,rs))
                   (mark `(,L ,mark ,R)))))
     (state-id .^= next-state-id))

    ({show-config}
     (let `(,L ,h ,R) tape.^)
     `(,@(reverse L) (,state-id.^ h) ,@R))
    ))

(to (peek marks)
  (match marks
    ('() '(-))
    (_ marks)))

;; TODO a canonicalizer that rewrites to an equiv turing machine
;; that steps one square at a time.
