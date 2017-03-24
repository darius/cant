;; A debugging console

(let logs (box<- '()))

(to (log format-string @arguments)
  (let m (call format-string `{.format ,@arguments}))
  (logs .^= `(,m ,@logs.^)))

(export log logs)
