;; printf-ish thing. TODO do something completely different?

(make format
  ((format-string @arguments)
   (really-format out format-string arguments))
  ({.to sink format-string @arguments}
   (really-format sink format-string arguments)))

(to (really-format sink format-string arguments)
  (begin scanning ((s format-string)
                   (args arguments))
    ;; XXX Maybe just make list patterns handle generic seqs.
    (match (as-list (s .slice 0 2))
      (()
       (unless args.empty?
         (error "Leftover arguments" args)))
      ((#\% #\w)             ;XXX actually design the format language
       (sink .print args.first)
       (scanning (s .slice 2) args.rest))
      ((#\% #\d)
       (sink .display args.first)
       (scanning (s .slice 2) args.rest))
      ((#\% #\%)
       (sink .display "%")
       (scanning (s .slice 2) args))
      ((#\% @_)
       (error "Bad format string" s))
      ((c @_)
       (sink .display c)
       (scanning s.rest args)))))

(export format)
