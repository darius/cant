(import (use "eg/kernel") eval global-env)


(to (run e)
  (print `(evaluating ,e))
  (print (eval (parse-exp e) global-env)))

;; TODO code duplicated from smoke-test.scm

(run '42)
(run ''hello)
; (run '(make _))
(run '((make (to xs xs))))
(run '((make (to xs xs)) 1 2 3))
(run '(if #no 1 2))
(run '(if #yes 1 2))
(run '((make (to '(#no) 'no) (to _ 'yes)) #no))
(run '((make (to '(#no) 'no) (to _ 'yes)) #yes))
(run '`(hello ,(if #yes 'yes 'no)))
(run '(2 .+ 3))
(run '(hide (let x 55)))
(run '(hide (to (f) 136) (f)))
(run '(hide
        (to (factorial n)
          (be n
            (0 1)
            (_ (n .* (factorial (- n 1))))))
        (factorial 10)))
