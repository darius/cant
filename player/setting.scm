(library (player setting)
(export setting? make-setting setting-a-list
        setting-binds? setting-extend-promises
        mutable-setting?
        )
(import (chezscheme) (player thing) (player env))

;; Wrapper for settings
;; The representation will change soon

(define-record-type setting (fields a-list))

(define (mutable-setting? setting)
  (null? (setting-a-list setting)))

(define (setting-binds? setting variable)
  (or (assq variable (setting-a-list setting))
      (global-defined? variable)))

;; TODO duplicate in player.scm
(define (setting-extend-promises setting vs)
  (let consing ((vs vs) (r (setting-a-list setting)))
    (if (null? vs)
        (make-setting r)
        (consing (cdr vs) (cons (cons (car vs) uninitialized) r)))))

)
