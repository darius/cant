;; Chez Scheme no longer always distinguishes input ports from output
;; ports, so we need a wrapper for one of those types just to tell
;; them apart. I choose to wrap the input ports.

(library (player source)
  (export source? make-source source-port make-source-maker
          source-read-char source-get-u8 source-read-all
          source-char-ready? close-source
          process/source)
(import (chezscheme)
  (player util)
  )

;; TODO always check that make-source is given a genuine port
(define-record-type source (fields port))

(define (make-source-maker maker)
  (lambda args
    (make-source (apply maker args))))

(define (source-read-char source)
  (read-char (source-port source)))

(define (source-get-u8 source)
  (get-u8 (source-port source)))

(define (source-read-all source)
  (let ((port (source-port source)))
    (let reading ((cs '()))
      (let ((c (read-char port)))
        (if (eof-object? c)
            (list->string (reverse cs))
            (reading (cons c cs)))))))

(define (source-char-ready? source)
  (char-ready? (source-port source)))

(define (close-source source)
  (close-port (source-port source)))

(define (process/source command)
  (let ((results (process command)))
    (insist (input-port? (car results)) "Unexpected type from `process`" results)
    (cons (make-source (car results))
          (cdr results))))

)
