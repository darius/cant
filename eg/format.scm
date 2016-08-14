(make format
  ((format-string @arguments)
   (call format `{.to ,out ,format-string ,arguments}))
  ({.to sink format-string arguments}   ;XXX can't have @arguments here yet
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
        (scanning s.rest args))))))

;; XXX belongs in stdlib if anywhere
(define (as-list seq)
  (if seq.empty?
      '()
      (cons seq.first (as-list seq.rest))))

(format "Dear %w,\n\nI wish to %d to 100%% of your %w.\n"
        "Archimedes" "subscribe" 'codices)
