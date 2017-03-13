;; Write structures with cyclic references cut short.
;; E.g.:
;; > (let a (box<- 42))
;; > (a .^= a)
;; > (cycle-write a)
;; #1=<box #1>

;; XXX just a sketch, untested too
;; TODO exclude known-to-be-acyclic types from the occurs table

(make cycle-write
  ((thing)
   (cycle-write thing out))
  ((thing sink)
   (let tags (map<-))
   (let buffer (fillvector<-))
   (let cycle-sink (cycle-sink<- tags buffer)) ;XXX better name?
   (cycle-sink .write thing)
   (for each! ((writer buffer.values))
     (writer sink))))

;; The tags table keeps a tag for each object the cycle sink
;; visits. The tag is 0 after the first visit; then, on the second and
;; thereafter, a positive integer identifying the object.

;; The buffer accumulates a sequence of procedures to send the final
;; formatted text to the destination sink.

(to (cycle-sink<- tags buffer)
  (let counter (box<- 0))
  (make cycle-sink
    ({.display str}                     ;XXX I guess? Is this the sink protocol?
     (buffer .push! (given (sink) (sink .display str)))
    ({.write thing}                     ;XXX or should this be .print?
     (let tag (tags .get thing #no))
     (case ((not tag)
            ;; First visit.
            (tags .set! thing 0)
            (buffer .push! (given (sink)
                             (let id (tags thing))
                             (unless (= 0 id)
                               (format .to sink "#~w=" id))))
            (thing .selfie cycle-sink))
           (else
            (let id (case ((= tag 0)
                           ;; Second visit.
                           (counter .^= (+ counter.^ 1)) ;TODO (incr counter) ?
                           (tags .set! thing counter.^)
                           counter.^)
                          (else
                           ;; Thereafter.
                           tag)))
            (buffer .push! (given (sink)
                             (format .to sink "#~w" id))))))
    ;; XXX .print?
    )))

(export cycle-write)
