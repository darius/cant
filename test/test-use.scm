(let left-pad-module (use "test/test-module-for-use"))
(import left-pad-module 
        left-pad)

(print (left-pad "  I am pretty padded.   "))

(let again (use "test/test-module-for-use")) ; Shouldn't print that it's loading again.
(surely (= again left-pad-module))
