;; (Use run.scm to run this.)

;; TODO: use parson instead? semantic feedback is awkward
(to (read-tree source)
  (begin parsing ()
    (let nc (read source)) (surely (not (eof? nc)))
    (let nm (read source)) (surely (not (eof? nm)))
    {node (for each/seq ((_ (1 .to nc)))
            (parsing))
          (for each/seq ((_ (1 .to nm)))
            (read source))}))

(to (each/seq f xs) ;force calls in left-to-right order. kinda embarrassing.
  (reverse (for foldl ((results '()) (x xs))
             `(,(f x) ,@results))))

(let input (with-input-file read-tree data-file))

(format "Part 1\n")

(to (sum-metadata {node children metadata})
  (+ (sum metadata)
     (sum-by sum-metadata children)))

(format "result 1: ~w\n" (sum-metadata input))


(display "\nPart 2\n")

(to (value<- {node children metadata})
  (sum (if children.none?
           metadata
           (for yeahs ((n metadata))
             (mayhap value<- (children .get n.-))))))

(format "result 2: ~w\n" (value<- input))

