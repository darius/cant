;; Equality and hashing

;; XXX comment below is irrelevant since switch from Gambit to Chez.
;; Needs a radical overhaul to use Chez's eq? hashtables

;; For now, I'm gonna assume Cant-defined objects are equal iff
;; eq?. This means you can't reconstitute an object from its script
;; and datum, which would be a reasonable implementation-level
;; operation for which cant=? would check if script and datum are
;; eq?, and hashing would also have to look at both.

(library (player equality)
(export hash cant=?
        mapi? mapi-items prim-mapi<-items prim-mapi-get
        )
(import (chezscheme) (player util) (player parse))

(define (hash x)
  (nat-hash    ;The Chez equal-hash doesn't mix up ints well
   (equal-hash x)))        ;XXX semantically wrong for Cant. quick hack to try out Chez.

;; From https://stackoverflow.com/a/12996028
;; unsigned int unhash(unsigned int x) {
;;     x = ((x >> 16) ^ x) * 0x119de1f3;
;;     x = ((x >> 16) ^ x) * 0x119de1f3;
;;     x = (x >> 16) ^ x;
;;     return x;
;; }
(define (nat-hash x)                    ;XXX undertested
  (define (mix x)
    (logxor x (ash x -16)))
  (define (mulmix x)
    (logand #xFFFFFFFF (* #x119de1f3 (mix x))))
  (mix (mulmix (mulmix x))))

(define (cant=? x y)
  (or (eqv? x y)
      (cond ((term? x)   (and (term? y) (term=? x y)))
            ((pair? x)   (and (pair? y) (pair=? x y)))
            ((string? x) (and (string? y) (string=? x y)))
            ((mapi? x)   (and (mapi? y) (cant=? (mapi-items x) (mapi-items y))))
            (else        #f))))

(define (pair=? x y)
  (and (cant=? (car x) (car y))
       (cant=? (cdr x) (cdr y))))

(define (term=? x y)
  (and (cant=? (term-tag x) (term-tag y))
       (let ((xs (term-parts x))
             (ys (term-parts y)))
         (and (= (length xs) (length ys))
              (let checking ((xs xs) (ys ys))
                (or (null? xs)
                    (and (cant=? (car xs) (car ys))
                         (checking (cdr xs) (cdr ys)))))))))


;; Immutable maps: placeholder implementation

(define-record-type mapi (fields items))

(define (prim-mapi<-items items) ;TODO handle item-lists that aren't concrete lists
  (make-mapi (mapi-dedup items)))

(define (mapi-dedup items)
  (let building ((already '())   ; Keys of the prefix up to here, sans duplicates
                 (suffix items)) ; Items not yet scanned
    (cond ((null? suffix) '())
          ((not (pair? suffix))
           (error '__mapi-dedup "Not a list of items" items))
          (else
           (let ((item (car suffix)))
             (unless (and (term? item)
                          (eq? (term-tag item) '~)
                          (length=2? (term-parts item)))
               (error '__mapi-dedup "Not an item" item))
             (let ((key (car (term-parts item))))
               (if (mem-cant=? key already)
                   (building already (cdr suffix))
                   (let ((built (building (cons key already) (cdr suffix))))
                     (if (eq? built (cdr suffix))
                         suffix
                         (cons item built))))))))))

(define (length=2? xs)
  (= (length xs) 2))      ;TODO don't have to compute the whole length

;; Is x among xs?
(define (mem-cant=? key xs)
  (memp (lambda (x) (cant=? key x)) xs))

(define (prim-mapi-get key mapi)
  (let searching ((items (mapi-items mapi)))
    (if (null? items)
        #f
        (let* ((item (car items))
               (k (car (term-parts item))))
          (if (cant=? key k)
              item
              (searching (cdr items)))))))


)
