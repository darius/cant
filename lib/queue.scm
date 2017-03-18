;; Functional queues.
;; Sequences with first-in-first-out addition and removal.

;; The pair {queue left right} represents the sequence (chain left
;; (reverse right)). Elements are removed on the left and added on the
;; right. The representation is split this way for amortized efficiency.

(let empty {queue '() '()})

(to (empty? {queue h t})
  (and h.empty? t.empty?))

(to (push {queue h t} element)
  {queue h `(,element ,@t)})

(to (extend {queue h t} elements)
  {queue h `(,@(reverse elements) ,@t)}) ;TODO chain-reverse

;; Return two values, `(,head ,rest-of-queue).
(to (dequeue {queue h t})               ;TODO rename to pop-front or something?
  (case (h.empty?
         (when t.empty?
           (error "Dequeue of empty queue"))
         (let seq (reverse t))
         `(,seq.first {queue ,seq.rest ()}))
        (else
         `(,h.first {queue ,h.rest ,t}))))

;; TODO come back to this:
;; Maybe a nicer accessor would look like:
(to (access {queue h t})
  (case ((and h.empty? t.empty?)
         {empty})
        (h.empty?
         (let seq (reverse t))
         {nonempty seq.first {queue seq.rest '()}))
        (else
         {nonempty h.first {queue h.rest t}))))

(export
  empty empty?
  push extend
  dequeue)
