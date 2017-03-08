(load "loadme.scm")

(let ((args (command-line)))
  (run-load (cadr args))
  (if (env-defined? repl-env 'main)
      (squeam-interpret `(main ',(cdr args)))))
