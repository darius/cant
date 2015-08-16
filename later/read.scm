;; A Lisp reader.
;; Planned extensions:
;; - Better error reporting
;; - Source-location tracking
;; - (Minor) new syntax like #yes/#no.

;; It's a drag to keep saying 'in-port' here, so let's just say 'port'.
;; TODO: better short names for in-port and out-port

;; XXX (read port default) is still not a POLA-respecting interface;
;; eof-object is better. But I'd prefer a uniform interface that's
;; like the answer for iterators, getting from a collection, etc.

(make eof)

(define (read-atom port char)
  XXX)

(let the-readtable (vector<-count 256 read-atom))

(define (set-read-macro char reader)
  (.set! the-readtable (.code char) reader))

(define (read port)
  (let char (.read-char port))
  (if (is? char eof)
      eof
      ((the-readtable (.code char)) port char)))

;; '(' tree* ')' -- where we've just eaten the '('.
;; XXX the extra must-read is ugly
(define (read-list port _)
  (recurse read-rest ()
    (skip-blanks port)
    (let peek (.peek-char port))
    (cond ((is? peek eof)
           (read-error port "Unexpected EOF in list"))
          ((is? peek #\) ) 
           (.read-char port)
           '())
          (else 
           (cons (must-read port) (read-rest))))))

(define (skip-blanks port) ;; and comments too
  XXX)

(define (read-number port radix)
  XXX)

(define (read-error port plaint)
  (error (chain "Read error: " plaint)))

;; White space
(for each! ((white '(#\space #\tab #\newline #\return)))
  (set-read-macro white 
    (given (port _) 
      (skip-blanks port)
      (read port))))

(set-read-macro #\;
  (given (port _)
    (flush-input-line port)
    (read port)))

(set-read-macro #\( read-list)

(set-read-macro #\)
  (given (port _)
    (read-error port "Unbalanced parentheses")))

(let radix-map '((#\x 16) (#\X 16)      ;TODO: hashmap
                 (#\d 10) (#\D 10)
                 (#\o  8) (#\O  8)
                 (#\b  2) (@\B  2)))

(set-read-macro #\#
  (given (port _)
    (let peek (.peek-char port))
    (cond ((assq peek radix-map) ;XXX can conflict with read-hash-symbol
           => (given (pair)
                (.read-char port)
                (read-number port (pair 1))))
          ((is? peek #\\)
           (.read-char port)
           (read-char-literal port))
          ((.alphabetic? peek)
           (read-hash-symbol (read-symbol port)))
          ((is? peek #\( )	; vector constant
           (list->vector (read-list port (.read-char port))))
          (else
           (read-error port "Unknown '#' read macro")))))

(set-read-macro #\"
  (given (port _)
    (recurse loop ((prev-chars '()))    ;TODO use growable-vector
      (let char (.read-char port))
      (cond ((is? char eof)
             (read-error port "Unexpected EOF in string constant"))
            ((is? char #\")
             (string<-list (reverse prev-chars)))
            ((is? char #\\)
             (let next (.read-char port))
             (cond ((is? next eof)
                    (read-error port "Unexpected EOF in escape sequence"))
                   ((assq next '((#\\ #\\)
                                 (#\" #\")
                                 (#\n #\newline)
                                 (#\t #\tab)
                                 (#\r #\return)))
                    => (given (pair)
		         (loop (cons (pair 1) prev-chars))))
                   (else
                    (read-error port 
                                "Unknown escape sequence in string"))))
            (else (loop (cons char prev-chars)))))))

(set-read-macro #\'
  (given (port _)
    `',(must-read port)))
    
(define (must-read port)
  (let tree (read port))
  (when (is? tree eof)
    (read-error port "Unexpected EOF"))
  tree)

(set-read-macro #\`
  (given (port _)
    (list<- 'quasiquote (must-read port))))

(set-read-macro #\,
  (lambda (port char)
    (list<- (cond ((is? (.peek-char port) #\@)
                   (.read-char port)
                   'unquote-splicing)
                  (else
                   'unquote))
            (must-read port))))
