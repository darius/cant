(let left-pad-module (use "eg/test-module-for-use.scm"))
(import left-pad-module 
        left-pad)

(print (left-pad "  I am pretty padded.   "))

(let again (use "eg/test-module-for-use.scm")) ; Shouldn't print that it's loading again.
(assert (= again left-pad-module))
