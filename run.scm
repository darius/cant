(load "loadme.scm")

(let ((args (command-line)))
  ;; TODO use load-and-run from stdlib
  (run-load (cadr args))
  (if (env-defined? repl-env 'main)
      (squeam-interpret `(main ',(cdr args)))))
