;; TODO: use parson instead? semantic feedback is awkward
(to (read-tree source)
  (begin parsing ()
    (let nc (read source)) (surely (not (eof? nc)))
    (let nm (read source)) (surely (not (eof? nm)))
    {node (for each/seq ((_ (range<- nc)))
            (parsing))
          (for each/seq ((_ (range<- nm)))
            (read source))}))

(to (each/seq f xs) ;force calls in left-to-right order. kinda embarrassing.
  (reverse (for foldl ((results '()) (x xs))
             `(,(f x) ,@results))))

(let input (with-input-file read-tree "advent08"))

(format "Part 1\n")

(to (sum-metadata {node children metadata})
  (+ (sum metadata)
     (sum (each sum-metadata children))))

(format "result 1: ~w\n" (sum-metadata input))


(display "\nPart 2\n")

(to (value<- {node children metadata})
  (if children.empty?
      (sum metadata)
      (sum (for each ((n metadata))
             (let child (children .get (- n 1)))
             (if child (value<- child) 0)))))

(format "result 2: ~w\n" (value<- input))

