(to (main)
  (print 0)
  (let pid (spawn screw-around))
  (monitor pid)                         ;does erlang have a spawn_monitor BIF?
  (? (msg (print "got it")
          (print msg))))

(to (screw-around)
  (print "yo")
  (throw "and a bottle of rum")
  (print "ho"))
