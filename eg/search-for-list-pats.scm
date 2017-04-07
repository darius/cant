;; OK, let's get an idea what search-squeam-code should look like,
;; from an actual application: finding instances of the pattern syntax
;; I'm about to outlaw. I.e., patterns like (a b) instead of `(,a ,b).

(import (use "lib/pretty-print") pp)
(import (use "lib/squeam-source-walker") expr-subparts patt-subparts)

(to (main `(,_ ,@filenames))
  (each! report-badness filenames))

(to (report-badness filename)
;;  (format "Checking ~d...\n" filename)
  (match (those bad-expr? (with-input-file read-all filename))
    ('() 'ok)
    (top-level-bad-exprs
     (format "In ~d these top-level expressions harbor badness:\n" filename)
     (each! pp top-level-bad-exprs)
     (newline))))

(to (bad-expr? expr)
  (bad-part? (expr-subparts expr)))

(to (bad-definition? arg es)
  (or (some bad-expr? es)
      ;; TODO just call bad-patt? after a step of macroexpansion
      (if (list? arg)
          (and (not arg.empty?)
               (match arg.last
                 ((list<- '@ p)
                  (or (bad-patt? p) (some bad-patt? ((reverse arg) .rest))))
                 (_ (some bad-patt? arg))))
          (bad-patt? arg))))

(to (bad-part? `(,subexprs ,subpatts))
  (or (some bad-expr? subexprs)
      (some bad-patt? subpatts)))

(to (bad-patt? patt)
  (or (and (cons? patt)
           (not ('(list<- cons quote quasiquote : @ optional) .find? patt.first))
           (do (format "This subpattern is bad: ~w\n" patt)
               #yes))
      (bad-part? (patt-subparts patt))))

(export main bad-expr? bad-patt?)
