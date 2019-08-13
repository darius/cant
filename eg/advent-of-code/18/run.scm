;; TODO load into a module instead of top-level

(load "eg/advent-of-code/utils.scm")

(to (main args)

  (let stem "eg/advent-of-code/18")
  (to (run code-name data-name)
    (squeam .play `(let data-name ',data-name)
                  '())
    (squeam .play `(let data-file ',("~d/data/~d" .format stem data-name))
                  '())
    (load ("~d/~d.scm" .format stem code-name)))

  (may args
    (be `(,_ ,filename)
      (run filename ("~d.in" .format filename)))
    (be `(,_ ,filename "-data" ,data-name)
      (run filename data-name))
    (be `(,me ,@_)
      (format "Usage: squeam ~d day [-data data]\n" me)
      (format "e.g.:  squeam ~d 06 -data 06.test\n" me)
      (os-exit 1))))
