;; XXX comment
;; ~/git/superbench/superopt/circuitoptimizer.scm

(load "stdlib.scm")

(define (superopt truth-table max-gates)
  (define n-inputs (int-log2 (.count truth-table)))
  (find-circuits (.parse-int truth-table 2) n-inputs max-gates))

(define (int-log2 n)
  ;; XXX ugly
  (if (is? n 1) 0
      (if (is? n 2) 1
          (if (is? n 4) 2
              (if (is? n 8) 3
                  (if (is? n 16) 4
                      (if (is? n 32) 5
                          (error "Bad argument" n))))))))

(define say
  (given arguments
    (each! display arguments)))

(define (pow2 n)
  (.<< 1 n))

(define (find-circuits wanted n-inputs max-gates)
  (define inputs (vector<-list (tabulate-inputs n-inputs)))
  (define mask (- (pow2 (pow2 n-inputs)) 1))

  (define (print-formula L-input R-input)
    (define v-name (chain (.slice "ABCDEF" 0 n-inputs)
                          (.slice "abcdefghijklmnopqrstuvwxyz" n-inputs)))
    (for each! ((i (range<- (.count L-input))))
      (define g (v-name (+ i n-inputs)))
      (define L (v-name (L-input i)))
      (define R (v-name (R-input i)))
      (say g " = " L " ~& " R "; "))
    (newline))

  (define (find-for-n n-gates)
    (say "Trying " n-gates " gates..." #\newline)
    (define n-wires (+ n-inputs n-gates))
    (define L-input (vector<-count n-gates #f))
    (define R-input (vector<-count n-gates #f))
    (define found?  (box<- #f))
    (define wire    (chain inputs L-input))
    (recurse sweeping ((gate 0))
      (for each! ((L (range<- (+ n-inputs gate))))
        (define L-wire (wire L))
        (.set! L-input gate L)
        (for each! ((R (range<- (+ L 1))))
          (define value (nand L-wire (wire R)))
          (.set! R-input gate R)
          (.set! wire (+ n-inputs gate) value)
          (cond ((< (+ gate 1) n-gates)
                 (sweeping (+ gate 1)))
                ((= wanted (.bit-and mask value))
                 (.set! found? #t)
                 (print-formula L-input R-input))))))
    (found?))

  (some? find-for-n (range<- 1 (+ max-gates 1))))

(define (nand x y)
  (.bit-not (.bit-and x y)))

(define (tabulate-inputs n-inputs)
  ;; An inputs vector is a vector of n-inputs bitvectors. It holds all
  ;; possible input patterns 'transposed': that is, the kth test case
  ;; can be formed out of bit #k of each the list's elements, one
  ;; element per circuit input. Transposed is the most useful form
  ;; because we can compute all test cases in parallel with bitwise
  ;; operators.
  (if (= n-inputs 0)
      '()
      (hide
       (define shift (pow2 (- n-inputs 1)))
       (cons (- (pow2 shift) 1)
             (for map ((iv (tabulate-inputs (- n-inputs 1))))
               (.bit-or iv (.<< iv shift)))))))
                   

(superopt "0110" 3)
(superopt "1011" 3)
;(superopt "0110" 4)
