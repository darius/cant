;; Write structures with cyclic references cut short.
;; E.g.:
;; > (let a (box<- 42))
;; > (a .^= a)
;; > (cycle-write a)
;; #1=<box #1>

;; Use cycle-sink<- if you want to perform multiple writes with id #s
;; keeping the same meaning.

;; TODO generally, sinks should not have a .close, right? POLA.
;; POLA is why we're splitting out the flush-to facet here, but if
;; we're going to go that route then string-sinks should be redesigned
;; in the same style.

;; TODO better names?

(to (cycle-write thing @(optional sink-arg))
  (let (_ cycle-sink flush-to) (cycle-sink<-))
  (cycle-sink .print thing)             ;TODO name it .write
  (flush-to (or sink-arg out))
  flush-to.close
  cycle-sink.close)

(to (cycle-sink<-)

  ;; The buffer accumulates a sequence of procedures to send the final
  ;; formatted text to the destination sink.
  ;; TODO I think this logic could be simpler
  ;;   e.g. use a flexarray of (either id thing) instead of procedures
  (let buffer (flexarray<-))

  (make flush-to

    (to _.string
      (with-output-string flush-to))

    (to (_ sink)
      (hey sink @buffer.values)
      buffer.clear!)

    ;; (not sure this is the best division of responsibilities; just trying it out)
    (to _.close
      buffer.clear!))

  ;; The tags map has a tag for each object the cycle-sink visits. The
  ;; tag is 0 after the first visit; then, on the second and thereafter,
  ;; a positive integer identifying the object.
  
  ;; This map is passed to cycle-sink<- so that multiple .print calls can
  ;; display the same id numbers for the same things, if that's your wish.
  ;; But 

  ;; TODO exclude known-to-be-acyclic types from the tags map

  (let tags (map<-))
  (let id-counter (box<- 0))    ; The highest unique id needed so far.

  (make cycle-sink

    (to (and (_ .display atom) message)
      (buffer .push! message))

    (to (and (_ .write-u8 u8) message)
      (buffer .push! message))

    (to (_ .print thing)
      (if (or (symbol? thing) (self-evaluating? thing))  ;; TODO skip other atom types
          (thing .selfie cycle-sink)
          (may (tags .get thing)
            (be #no
              ;; First visit.
              (tags .set! thing 0)
              (buffer .push! (on (sink)
                               ;;TODO avoid this extra lookup, via
                               ;;bookkeeping work on the second visit
                               (let id (tags thing))
                               (unless (= 0 id)
                                 (format .to-sink sink "#~w=" id))))
              (thing .selfie cycle-sink))
            (be tag
              (let id (may tag
                        (be 0   ;; Second visit. Give thing a fresh id:
                          (hey (id-counter .update _.+)
                               (-> (tags .set! thing it))))
                        (else   ;; Thereafter, just use that one:
                          tag)))
              (buffer .push! (on (sink)
                               (format .to-sink sink "#~w" id)))))))
  
    (to _.close
      tags.clear!))

  (_ cycle-sink flush-to))

(export cycle-write cycle-sink<-)
