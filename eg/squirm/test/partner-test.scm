;; TODO more tests
(to (main)
  (print 0)
  (let pid (spawn-partner screw-around))
  (? (msg (print "got it")
          (print msg))))

(to (screw-around)
  (print "yo")
  (throw "and a bottle of rum")
  (print "ho"))
