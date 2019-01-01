;; Find a smallest combinational circuit in NANDs for a boolean function.
;; From https://github.com/darius/superbench
;; Ultimately based on Kragen Sitaker's in C, but vectorized and
;; not doing don't-cares.

(to (superopt truth-table max-gates)
  (let n-inputs (int-log2 truth-table.count))
  (find-circuits (number<-string truth-table 2) n-inputs max-gates))

(to (int-log2 n)
  (begin searching ((p 0) (two**p 1))
    (match (compare two**p n)
      (-1 (searching (+ p 1) (* 2 two**p)))
      ( 0 p)
      (+1 (error "Not a power of 2" n)))))

(to (pow2 n)
  (1 .<< n))

(to (find-circuits wanted n-inputs max-gates)
  (let inputs (array<-list (tabulate-inputs n-inputs)))
  (let mask (- (pow2 (pow2 n-inputs)) 1))

  (to (print-formula L-input R-input)
    (let v-name (chain ("ABCDEF" .slice 0 n-inputs)
                       ("abcdefghijklmnopqrstuvwxyz" .slice n-inputs)))
    (for each! ((i L-input.keys))
      (let g (v-name (+ i n-inputs)))
      (let L (v-name (L-input i)))
      (let R (v-name (R-input i)))
      (format "~d = ~d ~~& ~d; " g L R))
    (newline))

  (to (find-for-n n-gates)
    (format "Trying ~w gates...\n" n-gates)
    (let n-wires (+ n-inputs n-gates))
    (let L-input (array<-count n-gates #no))
    (let R-input (array<-count n-gates #no))
    (let found?  (box<- #no))
    (let wire    (chain inputs L-input))
    (begin sweeping ((gate 0))
      (for each! ((L (0 .to< (+ n-inputs gate))))
        (let L-wire (wire L))
        (L-input .set! gate L)          ;XXX how about .:= or .<- or something?
        (for each! ((R (0 .to L)))
          (let value (nand L-wire (wire R)))
          (R-input .set! gate R)
          (wire .set! (+ n-inputs gate) value)
          (case ((< (+ gate 1) n-gates)
                 (sweeping (+ gate 1)))
                ((= wanted (mask .and value))
                 (found? .^= #yes)
                 (print-formula L-input R-input))))))
    found?.^)

  (some find-for-n (1 .to max-gates)))

(to (nand x y)
  ((x .and y) .not))     ;XXX why bitwise not here? only 1 bit, right?

(to (tabulate-inputs n-inputs)
  ;; An inputs array is an array of n-inputs bitvectors. It holds all
  ;; possible input patterns 'transposed': that is, the kth test case
  ;; can be formed out of bit #k of each the list's elements, one
  ;; element per circuit input. Transposed is the most useful form
  ;; because we can compute all test cases in parallel with bitwise
  ;; operators.
  (if (= n-inputs 0)
      '()
      (do (let shift (pow2 (- n-inputs 1)))
          (cons (- (pow2 shift) 1)
                (for each ((iv (tabulate-inputs (- n-inputs 1))))
                  (iv .or (iv .<< shift)))))))


(superopt "0110" 3)
(superopt "1011" 3)
;;(superopt "0110" 4)
