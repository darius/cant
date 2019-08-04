;; Ported from my old Scheme code.

(import (use 'pairing-heap) priority-queues<-)

(import (priority-queues<- <=)
  pq-empty? pq-min
  empty-pq unit-pq pq-insert pq-remove-min)

(to (grow-and-shrink numbers empty empty? get-min really-insert really-remove-min)

  (let history (flexarray<-))

  (to (insert pq elem)
    (let pq11 (really-insert pq elem))
    (history .push! (get-min pq11))
    pq11)

  (to (remove-min pq)
    (let pq1 (really-remove-min pq))
    (history .push! (if (empty? pq1) 'empty (get-min pq1)))
    pq1)

  (to (growing pq ns)
    (be ns
      ('() pq)
      (`(,n1) (insert pq n1))
      (`(,n1 ,n2 ,@rest)
       (growing (remove-min (insert (insert pq n1) n2))
                rest))))

  (begin shrinking ((pq (growing empty numbers))
                    (ns numbers))
    (if (empty? pq)
        history.values
        (shrinking (remove-min (remove-min (insert pq ns.first)))
                   ns.rest))))

(to (test-with numbers)

  (to (insert-sorted ns n)
    (if (or ns.empty? (<= n ns.first))
        (link n ns)
        (link ns.first
              (insert-sorted ns.rest n))))

  (let history-1
    (grow-and-shrink numbers 
                     empty-pq pq-empty? pq-min pq-insert pq-remove-min))
  (let history-2
    (grow-and-shrink numbers 
                     '() null? '.first insert-sorted '.rest))
  (if (= history-1 history-2)
      history-1
      (error "Test failed" `(,history-1 ,history-2))))

(to (test-each-tail numbers)
  (begin testing ((ns numbers))
    (test-with ns)
    (when #no
      (test-with (reverse ns))
      (unless ns.empty?
        (testing ns.rest)))))

(to (main _)
  (test-each-tail '(1))
  (test-each-tail '(0 0 0 0 0))
  (test-each-tail '(0 1 2 3 4 5))
  (test-each-tail
   '(3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3 2 3 8 4 6 2 6 4 3 3 8 3 2 7 9 
       5 0 2 8 8 4 1 9 7 1 6 9 3 9 9 3 7 5 1 0
       2 7 1 8 2 8 1 8 2 8 4 5 9 0 4 5))
  )
