#!chezscheme

;; Read Cant's variety of s-expressions. Different from Scheme's in
;; some ways, e.g. {tag x y}.

(library (player read)
(export cant-read)
(import (chezscheme) (player util))

;; This relies on char->integer returning a number in [0..255],
;; ASCII where that applies.

;; The grammar is succinct in the comments, in an ad-hoc PEG form.
;; The code is wordier, thanks to this conjunction of wishes:
;; - No backtracking, and one-character lookahead.
;; - Number syntax with decimal points, like 12.34
;; - Method-call shorthand syntax like 12.even?

;; Conventions:
;;   port   An input port
;;   c      A character (or possibly an EOF object).
;;          When c is a parameter to a read-foo function, it's
;;          ordinarily the last char read; you can use peek-char
;;          for lookahead.

;; TODO error reporting could be a lot better
;; TODO support Unicode

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

;; symbol: !digit[10] symbolconstituent+ !!symbolterminator afteratom

(define (read-symbol port c)
  (assert (not (char-numeric? c)))
  (read-after-atom (read-raw-symbol port (list c)) port))

(define (read-raw-symbol port L)
  (let ((c (peek-char port)))
    (cond ((symbol-terminator? c)
           (string->symbol (list->string (reverse L))))
          ((symbol-constituent? c)
           (read-raw-symbol port (cons (read-char port) L)))
          (else
            (error 'read "Weird character in symbol" c)))))

;; afteratom: cue*
;; cue:       '.' !digit[10] symbolconstituent+ !!symbolterminator
;; (This differs slightly from '.' followed by a symbol, because
;; we allow cues like '.+1' where '+1' on its own would be a number.)

(define (read-cue port dot-c)
  (when (char-numeric? (peek-char port))
    (error 'read "Illegal decimal-number syntax: integer part needed"))
  (read-cue-symbol port))

(define (read-cue-symbol port)
  (let ((sym (read-raw-symbol port (list #\.))))
    (when (eq? sym '|.|)
      (error 'read "Lone '.'"))
    sym))

(define (read-after-atom prefix port)
  (let ((c (peek-char port)))
    (if (eqv? c #\.)
        (read-after-atom (list prefix (read-cue port (read-char port)))
                         port)
        prefix)))

(define (read-after-atom-dot prefix port)
  (read-after-atom (list prefix (read-cue-symbol port))
                   port))

;; number:	('+'|'-')? (radix[R] digit[R]+ | decimal) afteratom
;; decimal:	digit[10]+ ('.' digit[10]+)?
;; digit[R]:	<digit of radix R>
;; radix[2]:	'0b'
;; radix[8]:	'0o'
;; radix[16]:	'0x'

(define (read-signed-or-symbol port sign-char)
  (let ((pc (peek-char port)))
    (if (char-numeric? pc)
        (apply-sign sign-char
                    (begin (read-char port) (read-unsigned port pc)))
        (read-symbol port sign-char))))

(define (apply-sign c number)
  (case c
    ((#\+) number)
    ((#\-) (- number))
    (else (error 'read "Bug: strange sign" c))))
    
(define (read-unsigned port c)
  (case c
    ((#\0) (case (peek-char port)
             ((#\b) (read-char port) (read-radix-unsigned 2 port))
             ((#\o) (read-char port) (read-radix-unsigned 8 port))
             ((#\x) (read-char port) (read-radix-unsigned 16 port))
             ((#\.) (read-decimal-fraction '(#\0) port))
             (else (read-unsigned-decimal port c))))
    (else (read-unsigned-decimal port c))))

(define (read-radix-unsigned radix port)
  (cook-number radix (read-digits radix port) port))

(define (cook-number radix literal-chars port)
  (read-after-atom (just-cook-number radix literal-chars) port))
  
(define (just-cook-number radix literal-chars)
  (let* ((literal (list->string literal-chars))
         (result (string->number literal radix)))
    (unless result
      (error 'read "Non-numeric number literal" literal))
    result))
  
(define (read-unsigned-decimal port c)
  (let* ((int-digits (cons c (read-digits 10 port))))
    (if (eqv? #\. (peek-char port))
        (read-decimal-fraction int-digits port)
        (cook-number 10 int-digits port))))

(define (read-decimal-fraction int-digits port)
  (read-char port) ;; TODO assert it's a '.'
  (let ((c (peek-char port)))
    (if (char-numeric? c)
        (cook-number 10
                     (append int-digits (cons #\. (read-digits 10 port)))
                     port)
        (read-after-atom-dot (just-cook-number 10 int-digits) port))))

(define (read-digits radix port)
  ;; TODO skip _ for readable grouping of digits
  (let loop ((rev-digits '()))
    (let ((c (peek-char port)))
      (cond ((or (char-numeric? c)
                 (and (= radix 16) (char-ci<=? #\a c #\f)))
             (read-char port)
             (loop (cons c rev-digits)))
            ((symbol-terminator? c)
             (reverse rev-digits))
            (else
              (error 'read "Non-numeric character in a number after" (reverse rev-digits)))))))

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

    ;; list: '(' expr* ')'
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
             (let* ((c (integer->char i))
                    (read-macro (cond ((char-numeric? c) read-unsigned)
                                      ((memv c '(#\+ #\-)) read-signed-or-symbol)
                                      ((symbol-constituent? c) read-symbol)
                                      (else #f))))
               (when read-macro
                 (vector-set! the-readtable i read-macro))
               (initializing (+ i 1))))))

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

    (install-read-macro #\. read-cue)

    (install-read-macro #\[
      (lambda (port c)
        (list->vector (read-seq #\] port c))))

    ;; hashliteral: '#\' (!!letter symbol | character) | '#yes' | '#no'
    (install-read-macro #\#
      (lambda (port c)
        (let ((next (read-char port)))
          (case next
            ((#\\)
             (let ((next (read-char port)))
               (if (and (char-alphabetic? next)
                        (char-alphabetic? (peek-char port)))
                   (let ((symbol (read-symbol port next)))
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
                    (let ((sym (read-symbol port next)))
                      (case sym
                        ((yes) #t)
                        ((no) #f)
                        (else
                         (read-error port "Unknown '#' read macro" sym)))))
                   (else
                    (read-error port "Unknown '#' read macro" next))))))))

    ;; stringliteral: '"' stringchar* '"' afteratom
    ;; stringchar: '\' escapeseq | character
    (install-read-macro #\"
      (lambda (port c)
	(let loop ((prev-chars '()))
	  (let ((c (read-char port)))
	    (cond
	     ((eof-object? c)
	      (read-error port "Unexpected EOF in string constant"))
	     ((char=? c #\")
	      (read-after-atom (list->string (reverse prev-chars)) port))
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
