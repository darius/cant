(import (use "eg/advent-of-code/utils")
  simple-parser<-)

(let input (with-input-file '.read-lines "eg/advent-of-code/18/data/advent12"))

(let initial-lineup (input.first .slice ("initial state: " .count)))

(let parser
  (simple-parser<- "{:skip :skip :skip :skip :skip} ' => ' :anyone"))
(to (parse string)
  ('.results (parser string)))

(let inputs (each parse input.rest.rest))


(display "\nPart 1\n")

(let rules
  (call set<- (for filter ((`(,pattern ,outcome) inputs))
                (match outcome
                  ("." #no)
                  ("#" pattern)))))

;; In retrospect, I could've used a vector instead of lib/hashset
;; (would've needed to track an offset for the left end, and if it got
;; big then left out the empty head and tail segments during
;; generate). Not really any simpler.
(to (state<-lineup lineup)
  (call set<- (for filter ((`(,pot ,ch) lineup.items))
                (and (= ch #\#) pot))))

(to (part1)
  (let state0 (state<-lineup initial-lineup))
  (let state (after-generations 20 state0))
  (sum state.keys))

(to (after-generations n state0)
  (for foldl ((state state0) (i (0 .up-to< n)))
    (format "~w: sum ~w\n" i (sum state.keys))
    (generate state)))

(to (generate state)
  (if state.empty?
      state
      (do (let `(,lo ,hi) (bounds-1d<- state.keys))
          (call set<- (for filter ((pot ((- lo 2) .up-to (+ hi 2))))
                        (generate-1 state pot))))))

(to (generate-1 state pot)
  (let key (string<- (at state (- pot 2))
                     (at state (- pot 1))
                     (at state pot)
                     (at state (+ pot 1))
                     (at state (+ pot 2))))
  (and (rules key)
       pot))

(to (at state pot)
  (if (state pot) #\# #\.))

(to (bounds-1d<- ns)
  `(,(call min ns) ,(call max ns)))

(format "~w\n" (part1))

;; part2 done by leting part1 run longer, extrapolating the sums by inspection
