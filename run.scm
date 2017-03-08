(load "loadme.scm")

(let ((args (command-line)))
  (run-load (cadr args))) ;TODO: provide args to Squeam code
