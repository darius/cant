;; (Use run.scm to run this.)

(let input (with-input-file _.read-all data-file))

(let parse
  (simple-parser<- ":nat ' players; last marble is worth ' :nat ' points\n'"))

(let `(,n-players ,n-points) (parse input))


(display "\nPart 1\n")

(to (play n m)                         ; n: #players; m: last-marble points
  (let circle (circle<- (+ m 1)))
  (let p (box<- 0))                     ; current marble position
  (let scores (array<-count n))
  (begin playing ((elf 0) (marble 1))
    (hm (when (23 .divides? marble)
          (print marble)
          (scores .set! elf (+ (scores elf) marble))
          (let other ((- p.^ 7) .modulo circle.count))
          (let removed (circle .pop! other))
          (scores .set! elf (+ (scores elf) removed))
          (p .^= other))
        (else
          ;; TODO speed up by initializing so no special cases
          (let pos (+ 1 (be circle.count
                          (1 0)
                          (2 0)
                          (_ ((+ p.^ 1) .modulo circle.count)))))
          (circle .insert! pos marble)
          (p .^= pos)))
    (unless (= marble m)
      (playing ((+ elf 1) .modulo n)
               (+ marble 1))))
  scores.values)

(to (circle<- N)
  (let V (array<-count N))
  (let L (box<- 1))
  (let H (box<- N))
  ;; The contents are in V from [0..L) and [H..N).

  ;; alternatives:
  ;; The contents are in V from [0..L) and [N-count-L..N).
  ;; The contents are in V from [0..L) and [L+gap..N).
  ;; The contents are in V from [0..L) and [N-R..N).

  (V .set! 0 0)

  (make circle
    ({.count}
     (+ L.^ (- N H.^)))

    ({.pop! i}
     (let lo L.^)
     (let hi H.^)
     (if (< i lo)
         (do (let popped (V i))
             (let j (+ i 1))
             (let k (- hi (- lo j)))
             ;; 0..i j..lo / hi..N
             (__vector-move! V k V j lo)
             ;; 0..i / hi-(lo-j)..hi..N
             ;;        gap+j..hi..N equivalently
             (L .^= i)
             (H .^= k)
             popped)
         (do (let k (+ hi (- i lo)))
             (let popped (V k))
             ;; 0..lo / hi..k..N
             (__vector-move! V lo V hi k)
             ;; 0..lo..i / k+1..N
             ;; 0..lo..i / gap+i+1..N equivalently
             (L .^= i)
             (H .^= (+ k 1))
             popped)))

    ({.insert! i value}
     (let lo L.^)
     (let hi H.^)
     (if (<= i lo)
         (do (let k (- hi (- lo i)))
             ;; 0..i..lo /        hi..N
             (__vector-move! V k V i lo)
             ;; 0..i / hi-(lo-i)..hi..N
             (V .set! i value)
             ;; 0....j / hi-(lo-i)..hi..N
             (L .^= (+ i 1))
             (H .^= k))
         (do (let k (+ hi (- i lo)))
             ;; 0..lo / hi..k..N
             ;; (__vector-move! V i V hi k)
             (__vector-move! V lo V hi k)
             ;; 0..lo..i / k..N
             ;; 0..lo..i / gap+i..N equivalently
             (V .set! i value)
             (L .^= (+ i 1))
             (H .^= k)
             )))

    ({.values}
     (chain (V .slice 0 L.^)
            (V .slice H.^)))
    ))

(let scores
; (play n-players n-points))
; (play n-players (* 100 n-points)))
 (play 10 25))
; (play 10 1618))
; (play 13 7999))
; (play 17 1104))
; (play 21 6111))
; (play 30 5807))

;(print scores)
(print (call max scores))


;(display "\nPart 2\n")
