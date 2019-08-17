;; A debugging console

(let logs (box<- '()))

(to (log format-string @arguments)
  (let m (format-string .format @arguments))
  (logs .^= `(,m ,@logs.^)))

(export log logs)
