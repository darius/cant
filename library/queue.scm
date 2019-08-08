;; Functional queues.
;; Sequences with first-in-first-out addition and removal.

;; The pair {queue left right} represents the sequence (chain left
;; (reverse right)). Elements are removed on the left and added on the
;; right. The representation is split this way for amortized efficiency.

(let empty {queue '() '()})

(to (empty? {queue h t})
  (and h.empty? t.empty?))

(to (push {queue h t} element)
  {queue h (link element t)})

(to (extend {queue h t} elements)
  {queue h (chain (reverse elements) t)}) ;TODO chain-reverse

(to (peek {queue h t})
  (hm (when (and h.empty? t.empty?)
        {empty})
      (when h.empty?
        (let seq (reverse t))
        {nonempty seq.first {queue seq.rest '()}})
      (else
        {nonempty h.first {queue h.rest t}})))

(to (list<-queue {queue h t})
  (chain h (reverse t)))

(export
  empty empty?
  push extend
  peek list<-queue)
