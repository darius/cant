;; (Fails on the real input.)

(import (use "eg/advent-of-code/utils")
  grammar<- parson-parse feed)
(import (use "lib/pretty-print")
  pp)
(import (use "lib/sort")
  sort-by-key)

;(let input (with-input-file '.read-all "advent24"))
(let input (with-input-file '.read-all "advent24.test"))

(to (battle)
  (format "Round: ~w vs. ~w\n" 
          (each '.count (armies "Infection"))
          (each '.count (armies "Immune System")))
  (unless (some '.empty? armies.values)
    (fight-round)
    (battle)))

(to (fight-round)
  (let a1 (select-targets "Infection"     infection immune-system))
  (let a2 (select-targets "Immune System" immune-system infection))
  (attacking (map<- (chain a1 a2)))
  (for each! ((`(,army ,groups) armies.items))
    (armies .set! army (those '.alive? groups))))

(to (select-targets my-name my-groups enemy-groups)
  (let result (flexarray<-))
  (let enemies (call flexarray<- enemy-groups)) ;; clumsy: probably ought to be a set
  (let enemy-nums (call flexarray<- (as-list (1 .up-to enemy-groups.count))))  ;; just for the messages
  (for each! ((`(,i ,group) (sort-by-key my-groups.items
                                         (given (`(,_ ,group)) group.target-selection-key))))
    (unless enemies.empty?
      (let damages (for each ((enemy enemies.values))
                     (group .would-damage enemy)))
;;      (for each! ((`(,j ,damage) damages.items))
;;        (format "~d group ~w would deal defending group ~w ~w damage\n"
;;                my-name (+ i 1) (enemy-nums j) damage))
      (let j (arg-max enemies.keys (given (j)
                                     (let enemy (enemies j))
                                     (list<- (damages j)   ;; or just compute it here
                                             enemy.effective-power
                                             enemy.initiative))))
      (when (< 0 (damages j))
        (result .push! `(,group ,(enemies j)))
        (enemies .pop! j)
        (enemy-nums .pop! j))))
  result.values)

(to (attacking selections)
  (for each! ((group (sort-by-key selections.keys (given (g) (- g.initiative)))))
    (when group.alive?
      (match (selections .get group)
        (#no)
        (target (group .attack! target))))))

(to (cook-group n-units hit-points qualities attack-damage attack-type initiative)
  (let immunities (set<-))
  (let weaknesses (set<-))
  (for each! ((`(,type ,attack-strings) qualities)) ;TODO could use a group-by or map-reduce again
    (let set (match type
               ("immune" immunities)
               ("weak"   weaknesses)))
    (set .add-all! (each symbol<- attack-strings)))
  (group<- n-units hit-points immunities weaknesses
           attack-damage (symbol<- attack-type) initiative))

(to (group<- initial-n-units hit-points immunities weaknesses attack-damage attack-type initiative)
  (let n-units (box<- initial-n-units))
  (to (effective-power) (* n-units.^ attack-damage))
  (surely (not (immunities .intersects? weaknesses)))  ; TODO method .disjoint?
  (let qualities (map<- (chain (for each ((immune immunities.keys)) `(,immune immunity))
                               (for each ((weak   weaknesses.keys)) `(,weak weakness)))))
  (make group
    ({.count} n-units.^)
    ({.effective-power} (effective-power))
    ({.initiative} initiative)
    ({.target-selection-key} (list<- (- (effective-power)) (- initiative)))
    ({.would-damage target}
     (target .damage-from attack-type (effective-power)))
    ({.attack! target}
     (target .receive! attack-type (effective-power)))
    ({.damage-from attacker-attack-type attack-power}
     (match (qualities .get attacker-attack-type)
       ('immunity 0)
       ('weakness (* 2 attack-power))
       (#no       attack-power)))
    ({.receive! attacker-attack-type attack-power}
     (let damage (group .damage-from attacker-attack-type attack-power))
     (let mortality (min n-units.^ (damage .quotient hit-points)))
     (n-units .^= (- n-units.^ mortality))
     (format "power ~w, damage ~w, killing ~w units, leaving ~w\n"
             attack-power damage mortality n-units.^))
    ({.alive?} (< 0 n-units.^))
    ({.show}
     (pp {group {size n-units.^}
                {hp hit-points}
                {immune-to immunities.keys}
                {weak-to weaknesses.keys}
                {attack attack-damage attack-type {initiative initiative}}}))))

(let grammar (grammar<- "
main: army**separator :end.
army: army_name ':\n' [group* :hug] :hug.
army_name: { (!':' :skip)* }.
group: :nat ' units each with ' :nat ' hit points ' opt_qualities 
       'with an attack that does ' :nat ' ' word 
       ' damage at initiative ' :nat '\n' :Group.
opt_qualities: '(' qualities ') ' | :hug.
qualities: quality_list++'; ' :hug.
quality_list: {'weak'|'immune'} ' to ' [word++', ' :hug] :hug.
word: :letter+ :join.  # TODO ugly
separator: '\n'.
"))
(let semantics (grammar (map<- `((Group ,(feed cook-group))))))
(let parse-main (semantics 'main))
(to (parse string)
  ('.results (parson-parse parse-main string)))

(let armies (map<- (parse input)))

(let immune-system (armies "Immune System"))
(let infection     (armies "Infection"))

(each! '.show immune-system)
(newline)
(each! '.show infection)

(display "\nPart 1\n")

(to (show-count)
  (for each! ((`(,army ,groups) (sort armies.items)))
    (format "~d:\n" army)
    (for each! ((`(,i ,group) groups.items))
      (format "Group ~w contains ~w units\n" (+ i 1) group.count))))

(show-count)

(to (part-1)
  (battle)
  (sum (for gather ((groups armies.values))
         (each '.count groups))))

(format "~w\n" (part-1))


(display "\nPart 2\n")

(to (part-2)
  'xxx)

(format "~w\n" (part-2))
