(to (main)
  (print 0)
  (print (catch
          (print "yo")
          (+ 'baloney 42)
          (print "ho"))))
