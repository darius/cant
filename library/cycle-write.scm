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
  (hey sink @buffer.values)
  void)
;; TODO restore if I decide I don't like 'hey':
;;  (for each! ((writer buffer.values))
;;    (writer sink)))

;; The tags map keeps a tag for each object the cycle sink visits. The
;; tag is 0 after the first visit; then, on the second and thereafter,
;; a positive integer identifying the object.

;; The buffer accumulates a sequence of procedures to send the final
;; formatted text to the destination sink.
;; TODO I think this logic could be simpler

(to (cycle-sink<- tags buffer)
  (let counter (box<- 0))
  (make cycle-sink

    (to (_ .display atom)
      (buffer .push! (_ .display atom)))

    (to (_ .print thing)
      (if (or (symbol? thing) (self-evaluating? thing))  ;; TODO skip other atom types
          (thing .selfie cycle-sink)
          (may (tags .get thing)
            (be #no
              ;; First visit.
              (tags .set! thing 0)
              (buffer .push! (on (sink)
                               (let id (tags thing))
                               (unless (= 0 id)
                                 (format .to-sink sink "#~w=" id))))
              (thing .selfie cycle-sink))
            (be tag
              (let id (if (= tag 0)
                          ;; Second visit:
                          (hey (counter .update _.+)
                               (-> (tags .set! thing it)))
                          ;; Thereafter:
                          tag))
              (buffer .push! (on (sink)
                               (format .to-sink sink "#~w" id)))))))))

(export cycle-write)
