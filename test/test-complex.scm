;; Complex numbers

(let cnum (use "lib/complex"))
(let (c+ c- c* c/ c-abs) (each cnum '(+ - * / abs)))

(let z {complex 0 0})
(let l {complex 1 0})
(let i {complex 0 1})

(let hypot (c+ (c- z (c- z (c+ l (c+ l l))))
               (c* i (c+ l (c+ l (c+ l l ))))))

(format "0+0 = %w\n" (c+ z z))
(format "i*i = %w\n" (c* i i))
(format "|3+4i| = %w\n" (c-abs hypot))
(format "z/z = %w\n" (c/ hypot hypot))
