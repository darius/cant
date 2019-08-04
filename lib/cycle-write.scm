;; Write structures with cyclic references cut short.
;; E.g.:
;; > (let a (box<- 42))
;; > (a .^= a)
;; > (cycle-write a)
;; #1=<box #1>

;; TODO exclude known-to-be-acyclic types from the tags map

(to (cycle-write thing @(optional sink-arg))
  (let sink (or sink-arg out))          ;TODO fancier (optional ...)
  (let tags (map<-))
  (let buffer (flexarray<-))
  (let cycle-sink (cycle-sink<- tags buffer)) ;XXX better name?
  (cycle-sink .print thing)
  (for each! ((writer buffer.values))
    (writer sink)))

;; The tags map keeps a tag for each object the cycle sink visits. The
;; tag is 0 after the first visit; then, on the second and thereafter,
;; a positive integer identifying the object.

;; The buffer accumulates a sequence of procedures to send the final
;; formatted text to the destination sink.
;; TODO I think this logic could be simpler

(to (cycle-sink<- tags buffer)
  (let counter (box<- 0))
  (make cycle-sink
    ({.display atom}
     (buffer .push! (_ .display atom)))
    ({.print thing}
     (if (or (symbol? thing) (self-evaluating? thing))  ;; TODO skip other atom types
         (thing .selfie cycle-sink)
         (be (tags .get thing)
           (#no
            ;; First visit.
            (tags .set! thing 0)
            (buffer .push! (on (sink)
                             (let id (tags thing))
                             (unless (= 0 id)
                               (format .to-sink sink "#~w=" id))))
            (thing .selfie cycle-sink))
           (tag
            (let id (hm (when (= tag 0)
                          ;; Second visit.
                          (counter .^= (+ counter.^ 1)) ;TODO (incr counter) ?
                          (tags .set! thing counter.^)
                          counter.^)
                        (else
                          ;; Thereafter.
                         tag)))
            (buffer .push! (on (sink)
                             (format .to-sink sink "#~w" id)))))))))

(export cycle-write)
