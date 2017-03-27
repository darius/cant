;; Functional, mergeable priority queues.
;;
;; A priority queue is a bag of ordered elements, with addition of
;; elements, removal of the minimum element, and merging of two
;; queues into one. The order of elements is given by a predicate
;; that's a curried argument to each operation; using different 
;; predicates on the same queue will produce unpredictable results.
;;
;; pq-min, pq-insert, and pq-merge are constant-time, while
;; pq-remove-min is conjectured to take logarithmic amortized time,
;; assuming a single thread of use; see Chris Okasaki's paper
;; "Functional Data Structures" for a way to handle general access
;; patterns efficiently. This code is adapted from that paper.
;; 
;; We represent a queue by a pairing heap: either the empty heap, or
;; a pair of the minimum element and a list of sub-heaps that merge
;; to form the overall heap. The sub-heaps must not be empty.

;; Ported from my old Scheme code.

;; XXX use (compare x y)
;; TODO compatible interface with fifo queues?

(to (priority-queues<- <=?)

  ;; The priority queue with no elements.
  (let empty-pq {pq})

  ;; True iff PQ is empty.
  (to (pq-empty? pq)
    (= pq empty-pq))

  ;; Return the minimum element of PQ.
  ;; Signals an error if PQ is empty.
  (to (pq-min pq)
    (match pq
      ({pq}        (error "pq-min of an empty pq"))
      ({pq elem _} elem)))

  ;; Return a priority queue with a single element, ELEM.
  (to (unit-pq elem)
    {pq elem '()})

  ;; Return a priority queue combining the elements of PQ1 and PQ2.
  (to (pq-merge pq1 pq2)
    (case ((pq-empty? pq1) pq2)
          ((pq-empty? pq2) pq1)
          (else (let {pq min1 rest1} pq1)
                (let {pq min2 rest2} pq2)
                (if (<=? min1 min2)
                    {pq min1 `(,pq2 ,@rest1)}
                    {pq min2 `(,pq1 ,@rest2)}))))

  ;; Return PQ with ELEM inserted.
  (to (pq-insert pq elem)
    (match pq
      ({pq}            {pq elem '()})
      ({pq min1 rest1} (if (<=? min1 elem)
                           {pq min1 `({pq ,elem ()} ,@rest1)}
                           {pq elem `(,pq)}))))

  ;; Return PQ with its minimum element removed.
  ;; Signals an error if PQ is empty.
  (to (pq-remove-min pq)
    (match pq
      ({pq} (error "pq-remove-min of empty-pq"))
      ({pq _ pqs}
       (begin merging ((pqs pqs))
         (match pqs
           (() empty-pq)
           ((elem) elem)
           ((pq1 pq2 @rest)
            (let {pq min1 rest1} pq1)
            (let {pq min2 rest2} pq2)
            (pq-merge (if (<=? min1 min2)
                          {pq min1 `(,pq2 ,@rest1)}
                          {pq min2 `(,pq1 ,@rest2)})
                      (merging rest))))))))

  (export
    pq-empty? pq-min
    empty-pq unit-pq pq-merge pq-insert pq-remove-min))

(export 
  priority-queues<-)
