;; Test use of quasiquote on term data.
;; (Already exercised sufficiently on plain old Lisp lists.)

(hide

 (let a '(yo there))
 (let b 'gee)

 (print `(ok {,b its cool ,@a b c} yay))
 (print `(ok {t its cool ,@a b c} yay))
 (print `(ok {t ,@a b c} yay))
 (print `(ok {t ,a b c} yay))
 (print `(ok {t a b c} yay))
 (print `(ok {t} yay))

)
;; TODO add hygiene test
