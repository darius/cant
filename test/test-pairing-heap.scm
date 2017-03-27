;; Ported from my old Scheme code.

(import (use "lib/pairing-heap")
  pq-empty? pq-min
  empty-pq unit-pq pq-insert pq-remove-min)

(let int-min        (pq-min <=))
(let int-insert     (pq-insert <=))
(let int-remove-min (pq-remove-min <=))

(to (grow-and-shrink numbers empty empty? get-min really-insert really-remove-min)

  (let history (fillvector<-))

  (to (insert pq elem)
    (let pq11 (really-insert pq elem))
    (history .push! (get-min pq11))
    pq11)

  (to (remove-min pq)
    (let pq1 (really-remove-min pq))
    (history .push! (if (empty? pq1) 'empty (get-min pq1)))
    pq1)

  (to (growing pq ns)
    (case (ns.empty? pq)
          (else
           (let pq1 (insert pq ns.first))
           (let rest ns.rest)
           (if rest.empty?
               pq1
               (growing (remove-min (insert pq1 rest.first)) 
                        rest.rest)))))

  (begin shrinking ((pq (growing empty-pq numbers))
                    (ns numbers))
    (if (empty? pq)
        (as-list history)
        (shrinking (remove-min (remove-min (insert pq ns.first)))
                   ns.rest))))

(to (test-with numbers)

  (to (insert-sorted ns n)
    (if (or ns.empty? (<= n ns.first))
        (cons n ns)
        (cons ns.first
              (insert-sorted ns.rest n))))

  (let history-1 
    (grow-and-shrink numbers 
                     empty-pq pq-empty? int-min int-insert int-remove-min))
  (let history-2 
    (grow-and-shrink numbers 
                     '() null? '.first insert-sorted '.rest))
  (if (= history-1 history-2)
      history-1
      (error "Test failed" `(,history-1 ,history-2))))

(to (test-each-tail numbers)
  (begin testing ((ns numbers))
    (test-with ns)
    (test-with (reverse ns))
    (unless ns.empty?
      (testing ns.rest))))

(to (main _)
  (test-each-tail '(1))
  (when #no
    (test-each-tail
     '(3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3 2 3 8 4 6 2 6 4 3 3 8 3 2 7 9 
         5 0 2 8 8 4 1 9 7 1 6 9 3 9 9 3 7 5 1 0
         2 7 1 8 2 8 1 8 2 8 4 5 9 0 4 5))
    (test-each-tail '(0 1 2 3 4 5))
    (test-each-tail '(0 0 0 0 0)))
  )
