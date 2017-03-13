;; Write structures with cyclic references cut short.
;; E.g.:
;; > (let a (box<- 42))
;; > (a .^= a)
;; > (cycle-write a)
;; #1=<box #1>

;; XXX just a sketch, untested too
;; XXX what about .display?

(make cycle-write
  ((thing)
   (cycle-write thing out))
  ((thing sink)
   (let occurs (map<-))
   (let cyclic? (box<- #no))
   (let buffer (fillvector<-))
   (let straw-sink (straw-sink<- occurs cyclic? buffer)) ;XXX better name?
   (straw-sink .write thing)
   (case (cyclic?.^
          (let cycle-sink (cycle-sink<- occurs sink))
          (cycle-sink .write thing))
         (else
          (flush buffer sink)))))

(to (flush buffer sink)
  (for each! ((str buffer.values))
    (sink .display str)))

;; The occurs table gives a value for each object the straw sink is
;; asked to write. The value is 0 if asked only once, else a positive
;; id if not yet visited in the 'real' writing pass, else negative.

(to (straw-sink<- occurs cyclic? buffer)
  (let counter (box<- 0))
  (make straw-sink
    ({.display str}                     ;XXX I guess?
     (buffer .push! str))
    ({.write thing}
     (let id (occurs .get thing #no))
     (case ((not id)
            (occurs .set! thing 0)
            (thing .selfie straw-sink))
           ((= id 0)
            (counter .^= (+ counter.^ 1)) ;TODO (incr counter) ?
            (occurs .set! thing counter.^)
            (cyclic .^= #yes))
           (else
            'ok)))
    ;; XXX ...
    ))

(to (cycle-sink<- occurs sink)
  (make cycle-sink
    ({.display str}                     ;XXX I guess?
     (sink .display str))
    ({.write thing}
     (let id (occurs thing))
     (case ((= id 0)
            (thing .selfie cycle-sink))
           ((< 0 id)
            (occurs .set! thing (- id))
            (sink .display ("#~w=" .format id))
            (thing .selfie cycle-sink))
           (else
            (sink .display ("#~w" .format (- id))))))
    ;; XXX ...
    ))

(export cycle-write)
