;; exports: start stop wait signal

;; This example is interesting in that the state I'd at first think of
;; as naturally living in the server processs (i.e. the list of
;; processes waiting on the mutex) is not represented in the server
;; loop's state arguments -- it's implicit in the process mailbox
;; instead.

(to (main)
  (start)
  (spawn (on ()
           (wait)
           (signal)))
  (wait)
  (signal)
  ;; TODO more
  (print "done"))

(to (start)
  (register 'mutex (spawn free)))

(to (stop)
  (! 'mutex 'stop))

(to (wait)
  (! 'mutex ['wait (me)])
  (? ('ok 'ok)))

(to (signal)
  (! 'mutex ['signal (me)])
  'ok)

(to (free)
  (? (['wait pid]
      (! pid 'ok)
      (? (['signal (: pid)]
          (free))))
     ('stop
      (begin killing ()
        (? (['wait pid]
            (exit pid 'kill)
            (killing))
           ((after 0)
            'ok))))))
