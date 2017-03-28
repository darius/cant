;; Ported from: _Metamagical Themas_ by Douglas Hofstadter, 

;; N.B. expressing probabilities as a fraction of 1000, since we don't yet
;; do floating point numbers.

(import (use "lib/parson-core")         parse)
(import (use "lib/parson") grammar<-)
(import (use "lib/text-wrap")      fill)

(to (main (_ @words))
  (let expanded (expand-text (" " .join words) 1000))
  (format "~d\n" (fill expanded 72)))

(to (expand-text text probability)
  (call chain
        (for each ((token ((parse parser text) .opt-results)))
          (expand token probability))))

(let grammar (grammar<- "
phrase: (word | :anyone)*.
word:   :letter+ {'!'}? :join.
"))
(let parser ((grammar (map<-)) "phrase"))

(to (expand token probability)
  (if (and (menu .maps? token) (< (random-integer 1000) probability))
      (expand-text (menu token) (/ (* probability 4) 5))
      token))

(let menu
  (map<-
   '(("tomatoes" "tomatoes on macaroni (and tomatoes only), exquisitely spiced")
     ("macaroni" "macaroni and cheese (a repast of Naples, Italy)")
     ("repast" "rather extraordinary pasta and sauce, typical")
     ("cheese" "cheddar, havarti, Emmentaler (particularly sharp Emmentaler)")
     ("sharp" "strong, hearty, and rather pungent")
     ("spiced" "sweetly pickled in cheese endive dressing")
     ("endive" "egg noodles, dipped in vinegar eggnog")
     ("noodles" "noodles (oodles of delicious linguini), elegantly served")
     ("linguini" "lambchops (including noodles), got usually in northern Italy")
     ("pasta" "pasta and sauce (that's all!)")
     ("all!" "a luscious lunch")
     ("sauce" "shad and unusual coffee (eccellente!)")
     ("shad" "spaghetti, heated al dente")
     ("spaghetti" "standard pasta, always good, hot especially (twist, then ingest)")
     ("coffee" "choice of fine flavors, particularly espresso")
     ("espresso" "excellent, strong, powerful, rich espresso, suppressing sleep outrageously")
     ("basta!" "belly all stuffed (tummy ache!)")
     ("lambchops" "lasagne and meatballs, casually heaped onto pasta sauce")
     ("lasagne" "linguini and sauce and garlic (noodles everywhere!)")
     ("rhubarb" "ravioli heated under butter and rhubarb (basta!)")
     ("ravioli" "rigatoni and vongole in oil, lavishly introduced")
     ("rigatoni" "rich Italian gnocchi and tomatoes (or noodles instead)")
     ("gnocchi" "garlic noodles over crisp cheese, heated immediately")
     ("garlic" "green and red lasagne in cheese")
     )))

(export main expand-text expand)
