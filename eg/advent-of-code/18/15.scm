(import (use "lib/queue")
  empty empty?                          ;TODO too-generic names
  push extend
  peek)

;(let input (with-input-file '.read-all "advent15.test"))
;(let input (with-input-file '.read-all "advent15.movement"))
;(let input (with-input-file '.read-all "advent15.movement2"))
;(let input (with-input-file '.read-all "advent15.combat1"))
;(let input (with-input-file '.read-all "advent15.combat2"))
;(let input (with-input-file '.read-all "advent15.combat3"))
;(let input (with-input-file '.read-all "advent15.combat4"))
;(let input (with-input-file '.read-all "advent15.combat5"))
(let input (with-input-file '.read-all "advent15"))

(to (part-1)
  (let field (field<- input 3))
  (let n-rounds
    (begin battling ((t 0))
      (show t field)
      (case (field.do-round! (battling (+ t 1)))
            (else t))))
  (let hp field.total-hit-points)
  (format "Done after ~w rounds with ~w hit points left.\n"
          n-rounds hp)
  (* n-rounds hp))

(to (show t field)
  (format "After ~w rounds:\n" t)
  field.show
  (newline))

(to (field<- field-string elf-attack-power)

  (let width (+ 1 field-string.split-lines.first.count))
  (surely (for every ((line field-string.split-lines))
            (= width (+ line.count 1))))

  (let area (array<-list (as-list field-string)))

  (to (coords<- p)
    ("~w,~w" .format (p .quotient width) (p .modulo width)))

  ;; Step in directions N E S W
  (let steps (array<- (- width) 1 width -1)) ;TODO maybe a list instead?

  (to (neighbors<- p)
    (for each ((s steps))
      (+ p s)))

  (to (open-neighbors<- p)
    (for those ((q (neighbors<- p)))
      (= (area q) #\.)))

  ;; A unit is a goblin or elf.
  (to (unit? char)
    ("GE" .find? char))

  ;; Map from each remaining unit's position to its hit points,
  ;; initially 200. (A unit's species is known by its G or E on the
  ;; map.)
  (let units 
    (map<- (for filter ((`(,p ,ch) area.items))
             (and (unit? ch)
                  `(,p 200)))))

  ;; For each unit, do a turn. Return yes if the round completed.
  (to (do-round)
    (begin doing ((items (sort units.items)))
      (or items.empty?
          (do (let `(,p ,unit) items.first)
              ;; This unit may have been killed in a preceding unit's turn,
              ;; but it can't have moved yet.
              (if (units .maps? p)
                  (and (do-turn p)
                       (doing items.rest))
                  (doing items.rest))))))

  ;; For the unit at p, do a turn. Return yes if it found a target.
  (to (do-turn p)
    (let targets (find-targets (area p)))
    (unless (maybe-attack p)
      ;;  (format "targets for ~w: ~w\n" p (sort targets))
      (let goals (call set<- (gather open-neighbors<- targets)))
      (match (find-shortest-paths p goals)
        ('())
        (paths
         (let chosen-path
           (min-by (compound-key<- '.last '.first) paths))
         (let spot chosen-path.first)
         (move! p spot)
         (unless (maybe-attack spot)
           (format "unit ~d did not attack\n" (coords<- spot))))))
    (not targets.empty?))

  (to (find-targets species)
    (for filter ((q units.keys))
      (and (not= species (area q))
           q)))

  (to (maybe-attack p)
    (let enemy (enemy<- (area p)))
    (match (for those ((q (neighbors<- p)))
             (= enemy (area q)))
      ('() #no)
      (targets
       (let target
         (min-by (given (target)
                   ;; The least remaining hit points, then the first in reading order.
                   `(,(units target) ,target))
                 targets)
       ;;     (format "target ~w\n" target)
       (attack p target)
       #yes)))

  (to (attack p target)
    (let hp-left (- (units target)
                    (match (area p)
                      (#\G 3)
                      (#\E elf-attack-power))))                      
    (format "attack: from ~d@~d to ~d@~d leaving ~w hp\n"
            (area p) (coords<- p) (area target) (coords<- target) hp-left)
    (case ((<= hp-left 0)
           (units .delete! target)
           (area .set! target #\.))
          (else
           (units .set! target hp-left))))

  (to (move! from-p to-p)
    (let species (area from-p))
    (let hp (units from-p))
    (units .delete! from-p)
    (area .set! from-p #\.)
    (units .set! to-p hp)
    (area .set! to-p species))

  (to (find-shortest-paths start-pos goals)
    ;; Simple breadth-first search
    (let successes (flexarray<-))
    (let already (set<-)) ; Spots not to enqueue again.

    (to (expand trail)
      (for filter ((q (sort (open-neighbors<- trail.first))))
        (and (not (already q))
             (do (already .add! q)
                 `(,q ,@trail)))))

    (begin exploring ((queue
                       (extend empty (for each ((trail (expand `(,start-pos))))
                                       `(,trail.first)))))
;;      (surely (sorted? (for each ((trail (list<-queue queue)))
;;                         `(,trail.count ,(reverse trail)))))
      (match (peek queue)
        ({empty}
         (each reverse successes.values))
        ({nonempty trail queue-1}
         (let spot trail.first)
         (if (goals .maps? spot)
             (if (and (not successes.empty?)
                      (< successes.first.count trail.count))
                 (each reverse successes.values)
                 (do (successes .push! trail)
                     (exploring (extend queue-1 (expand trail)))))
             (exploring (extend queue-1 (expand trail))))))))

  (to (list<-queue {queue h t})
    (chain h (reverse t)))

  (to (sorted? xs)
    (or xs.empty?
        xs.rest.empty?
        (and (<= xs.first xs.rest.first)
             (sorted? xs.rest))))
  
  (to (show)
    (let notes (flexarray<-))
    (for each! ((`(,p ,ch) area.items))
      (match ch
        (#\newline
         (format "   ~d\n" (", " .join notes.values))
         notes.clear!)
        (else
         (when (unit? ch)
           (notes .push! ("~d(~w)" .format ch (units p))))
         (display ch)))))

  (make field
    ({.do-round!}        (do-round))
    ({.show}             (show))
    ({.total-hit-points} (sum units.values))
    ({.census}           (call bag<- area.values))
    ))

(let enemy<- (map<- '((#\G #\E)
                      (#\E #\G))))

;(format "Part 1 answer: ~w\n" (part-1))



(to (part-2)
  (display "\nPart 2\n")

  (begin trying ((power 4))
    (newline)
    (format "Trying attack power ~w\n" power)
    (let field (field<- input power))
    (match (battle-sans-elf-casualties field power)
      (#no (trying (+ power 1)))
      (outcome outcome))))

(to (battle-sans-elf-casualties field power)
  (let n-elves (field.census .get #\E))
  (begin battling ((t 0))
    (show t field)
    (case ((not= n-elves (field.census .get #\E))
           (format "Elf casualties, aborting the battle\n\n")
           #no)
          (field.do-round! (battling (+ t 1)))
          (else
           (let hp field.total-hit-points)
           (format "Done after ~w rounds with ~w hit points left.\n"
                   t hp)
           (* t hp)))))

(format "~w\n" (part-2))
