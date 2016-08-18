;; From https://github.com/darius/superbench
;; Ultimately based on Kragen Sitaker's in C, but vectorized and
;; not doing don't-cares.

(define (superopt truth-table max-gates)
  (let n-inputs (int-log2 truth-table.count))
  (find-circuits (number<-string truth-table 2) n-inputs max-gates))

(define (int-log2 n)
  ;; XXX ugly
  (match n
    (1  0)
    (2  1)
    (4  2)
    (8  3)
    (16 4)
    (32 5)
    (_ (error "Bad argument" n))))

(define (say @arguments)
  (each! display arguments))

(define (pow2 n)
  (1 .<< n))

(define (find-circuits wanted n-inputs max-gates)
  (let inputs (vector<-list (tabulate-inputs n-inputs)))
  (let mask (- (pow2 (pow2 n-inputs)) 1))

  (define (print-formula L-input R-input)
    (let v-name (chain ("ABCDEF" .slice 0 n-inputs)
                       ("abcdefghijklmnopqrstuvwxyz" .slice n-inputs)))
    (for each! ((i L-input.keys))
      (let g (v-name (+ i n-inputs)))
      (let L (v-name (L-input i)))
      (let R (v-name (R-input i)))
      (say g " = " L " ~& " R "; "))
    (newline))

  (define (find-for-n n-gates)
    (say "Trying " n-gates " gates..." #\newline)
    (let n-wires (+ n-inputs n-gates))
    (let L-input (vector<-count n-gates #no))
    (let R-input (vector<-count n-gates #no))
    (let found?  (box<- #no))
    (let wire    (chain inputs L-input))
    (begin sweeping ((gate 0))
      (for each! ((L (range<- (+ n-inputs gate))))
        (let L-wire (wire L))
        (L-input .set! gate L)          ;XXX how about .:= or .<- or something?
        (for each! ((R (range<- (+ L 1))))
          (let value (nand L-wire (wire R)))
          (R-input .set! gate R)
          (wire .set! (+ n-inputs gate) value)
          (case ((< (+ gate 1) n-gates)
                 (sweeping (+ gate 1)))
                ((= wanted (mask .and value))
                 (found? .^= #yes)
                 (print-formula L-input R-input))))))
    found?.^)

  (some find-for-n (range<- 1 (+ max-gates 1))))

(define (nand x y)
  ((x .and y) .not))     ;XXX why bitwise not here? only 1 bit, right?

(define (tabulate-inputs n-inputs)
  ;; An inputs vector is a vector of n-inputs bitvectors. It holds all
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
;(superopt "0110" 4)
