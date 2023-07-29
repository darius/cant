#!chezscheme

;; Read Cant's variety of s-expressions. Different from Scheme's in
;; some ways, e.g. {tag x y}.

(library (player read)
(export cant-read)
(import (chezscheme) (player util))

;; Adapted from my UTS Scheme reader.
;; This relies on char->integer returning a number in [0..255].

;; Conventions:
;;   port   an input port
;;   c      a character (or possibly an EOF object)

;; TODO support Unicode
;; XXX error reporting could be a lot better
;; XXX symbol as anything that doesn't parse as a number, that's error-prone

(define (flush-input-line port)
  (let loop ()
    (let ((c (read-char port)))
      (cond ((eof-object? c))
	    ((char=? c #\newline))
	    (else (loop))))))

(define (skip-blanks port)
  (let loop ()
    (let ((c (peek-char port)))
      (cond ((eof-object? c))
	    ((char-whitespace? c)
	     (read-char port)
	     (loop))
	    ((char=? c #\;)
	     (flush-input-line port)
	     (loop))))))

(define (symbol-constituent? c)
  (or (char-alphabetic? c)
      (char-numeric? c)
      (memv c '(#\~ #\! #\$ #\% #\^ #\& #\* #\/ #\? #\= #\+ #\- #\_ #\< #\> #\:))))

(define (symbol-terminator? c)
  (or (eof-object? c)
      (char-whitespace? c)
      (memv c '(#\. #\| #\( #\) #\; #\" #\' #\` #\, #\@ #\{ #\} #\[ #\]))))

;; Here 'atom' means symbol or number. TODO just separate those cases
(define (read-atom port c)
  (read-after-atom (read-raw-atom port (list c)) port))

(define (read-raw-atom port L)
  (let ((c (peek-char port)))
    (if (symbol-terminator? c)
        (atomize L)
        (read-raw-atom port (cons (read-char port) L)))))

(define (read-after-atom prefix port)
  (let ((c (peek-char port)))
    (cond ((eqv? c #\.)
           (read-char port)
           (let ((c (peek-char port)))
             (if (not (symbol-constituent? c))
                 (error 'read "Symbol ending in '.'"))
             (read-after-atom (combine-dotted prefix (read-raw-atom port (list (read-char port))))
                              port)))
          (else prefix))))
  
(define (atomize L)
  (let ((s (list->string (reverse L))))
    (or (string->number s)
        (prefixed-string->number "0x" 16 s)
        (prefixed-string->number "0o"  8 s)
        (prefixed-string->number "0b"  2 s)
        (string->symbol s))))

(define (prefixed-string->number prefix radix s)
  (let ((pn (string-length prefix))
        (sn (string-length s)))
    (and (string=? prefix (substring s 0 (min pn sn)))
         (string->number (substring s pn sn) radix))))

(define (combine-dotted prefix atom)
  (cond ((not prefix) atom)
        ((and (integer? prefix) (integer? atom))  ;; XXX plays wrong with radix prefixes
         ;;ugh. This whole idea of reading atoms and then combining them was a hack I've gotta back out of.
         (string->number (string-append (number->string prefix)
                                        "."
                                        (number->string atom))))
        ((symbol? atom)
         (list prefix (cue<- atom)))
        (else
          (error 'read "Floating-point suffix after a non-number prefix" prefix atom))))

(define cant-read
  (let ()

    (define (read-error port plaint . irritants)
      (apply error 'read plaint irritants))

    (define (unknown-char port c)
      (read-error port "Unknown read syntax" c))

    (define the-readtable (make-vector 128 unknown-char))

    (define (install-read-macro c reader)
      (vector-set! the-readtable (char->integer c) reader))

    (define (read port)
      (let ((c (read-char port)))
	(if (eof-object? c)
	    c
	    ((vector-ref the-readtable (char->integer c)) 
	     port 
	     c))))

    (define (must-read port)
      (let ((result (read port)))
        (if (eof-object? result)
            (read-error port "Unexpected EOF"))
        result))

    ;; list ::= '(' expr* ')'
    (define (read-list port c)
      (read-seq #\) port c))

    (define (read-seq close-paren port c)
      (let reading ()
        (skip-blanks port)
        (let ((c (peek-char port)))
          (cond ((eof-object? c) 
                 (read-error port "Unexpected EOF before" close-paren))
                ((char=? c close-paren)
                 (read-char port)
                 '())
                (else
                 (cons (must-read port) (reading)))))))

    ;; TODO remove this (and term<-list) if we switch to the new term syntax
    (define (read-term port c)
      (let ((enclosed (read-seq #\} port c)))
        (if (null? enclosed)
            (read-error port "Missing tag in {} term")
            (term<-list enclosed))))

    (let initializing ((i 33)) ;; Symbol/number characters
      (cond ((< i 127)
             (if (symbol-constituent? (integer->char i))
                 (vector-set! the-readtable i read-atom))
             (initializing (+ i 1)))))

    (for-each (lambda (white) ;; Whitespace characters
                (install-read-macro white 
                                    (lambda (port c) 
                                      (skip-blanks port)
                                      (read port))))
              '(#\space #\tab #\newline #\return))

    (install-read-macro #\;
      (lambda (port c)
	(flush-input-line port)
	(read port)))

    (install-read-macro #\( read-list)

    (install-read-macro #\)
      (lambda (port c)
	(read-error port "Too many ')'")))

    (install-read-macro #\{ read-term)

    (install-read-macro #\}
      (lambda (port c)
	(read-error port "Too many '}'")))

    (install-read-macro #\.
      (lambda (port c)
        (skip-blanks port)
        (let ((c (peek-char port)))
          (if (symbol-constituent? c)
              (let ((atom (read-atom port (read-char port))))
                (cond ((number? atom)
                       (read-error port "Floating-point literals require an int part"))
                      ((symbol? atom)
                       (cue<- atom))
                      (else
                       (read-error port "Bad syntax after '.'" atom))))
              (read-error port "Lone '.'" c)))))

    (install-read-macro #\[
      (lambda (port c)
        (list->vector (read-seq #\] port c))))

    (install-read-macro #\#
      (lambda (port c)
        (let ((next (read-char port)))
          (case next
            (( #\( )	; new term notation -- TODO unused, just drop it?
             (let* ((tag (must-read port))
                    (parts (read-seq #\) port c)))
               (make-term tag parts)))
            ((#\\)
             (let ((next (read-char port)))
               (if (and (char-alphabetic? next)
                        (char-alphabetic? (peek-char port)))
                   (let ((symbol (read-atom port next)))
                     (let ((table '(; (backspace . #\backspace)
				    ; (escape . #\escape)
				    ; (page . #\page)
                                    (newline . #\newline)
                                    (return . #\return)
                                    (space . #\space)
                                    (tab . #\tab)
                                    )))
                       (let ((pair (assq symbol table)))
                         (if (pair? pair)
                             (cdr pair)
                             (read-error port 
                                         "Unknown character constant - #\\"
                                         symbol)))))
                   next)))
            (else
             (cond ((char-alphabetic? next)
                    (let ((sym (read-atom port next)))
                      (case sym
                        ((yes) #t)
                        ((no) #f)
                        (else
                         (read-error port "Unknown '#' read macro" sym)))))
                   (else
                    (read-error port "Unknown '#' read macro" next))))))))

    (install-read-macro #\"
      (lambda (port c)
	(let loop ((prev-chars '()))
	  (let ((c (read-char port)))
	    (cond
	     ((eof-object? c)
	      (read-error port "Unexpected EOF in string constant"))
	     ((char=? c #\")
	      (list->string (reverse prev-chars)))
	     ((char=? c #\\)
	      (let ((c (read-char port)))
		(cond
		 ((eof-object? c)
		  (read-error port "Unexpected EOF in escape sequence"))
                 ((char=? c #\newline)
                  (loop prev-chars))
		 ((assv c '((#\\ . #\\)
                            (#\" . #\")
                            (#\n . #\newline)
                            (#\t . #\tab)
                            (#\r . #\return)))
		  => (lambda (pair)
		       (loop (cons (cdr pair) prev-chars))))
		 (else (read-error port 
				   "Unknown escape sequence in string")))))
	     (else (loop (cons c prev-chars))))))))

    (install-read-macro #\'
      (lambda (port c)
	(list 'quote (must-read port))))
    
    (install-read-macro #\`
      (lambda (port c)
	(list 'quasiquote (must-read port))))

    (install-read-macro #\,
      (lambda (port c)
	(list 
	 (if (eqv? (peek-char port) #\@)
	     (begin (read-char port) 'unquote-splicing)
	     'unquote)
	 (must-read port))))

    (install-read-macro #\@
      (lambda (port c)
        (list '@ (must-read port))))    ;XXX for now

    (install-read-macro #\|
      (lambda (port c)
        (let ((next (read-char port)))
          (case next
            ((#\|)                      ; ||foo --> (on (it) foo)
             `(on (it) ,(must-read port)))
            (else
              (read-error port "Unknown '|' read macro" next))))))

    read))

)
