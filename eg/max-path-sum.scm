;; Maximum sum of values along any path in a binary tree.
;; tree ::= {leaf} | {branch value tree tree}

(to (max-path-sum tree)
  (let (best best-to-root) (solve tree))
  best)

(to (solve tree)
  (match tree
    ({leaf}
     '(0 0))
    ({branch value L R}
     (let (best-L best-to-root-L) (solve L))
     (let (best-R best-to-root-R) (solve R))
     (let best-through-root (+ best-to-root-L value best-to-root-R))
     (let best-to-root (+ value (max best-to-root-L best-to-root-R)))
     (list<- (max best-L best-R best-through-root)
             (max 0 best-to-root)))))

(print (max-path-sum
        {branch -10
                {branch 2 {branch 3 {leaf} {leaf}} {branch -1 {leaf} {leaf}}}
                {branch 3 {leaf} {leaf}}}))
