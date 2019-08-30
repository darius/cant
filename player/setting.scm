(library (player setting)
(export setting? make-setting setting-a-list
        setting-binds? setting-extend-promises
        mutable-setting?
        setting/missing
        setting-lookup global-lookup
        setting-resolve!
        )
(import (chezscheme) (player thing) (player env))

;; Wrapper for settings
;; The representation will change soon

(define-record-type setting (fields a-list))

(define setting/missing (list '*missing*))

(define (setting-lookup r variable)     ;r is the a-list of a setting, for now
  (cond ((assq variable r) => cdr)
        (else (global-lookup variable))))

(define (global-lookup v)
  (let ((value (really-global-lookup v)))
    (if (eq? value missing)
        setting/missing
        value)))

;; Return #f on success, else a complaint.
(define (setting-resolve! r name value) ;r is the a-list of a setting, for now
  (cond ((assq name r)
         => (lambda (pair)
              (if (eq? (cdr pair) uninitialized)
                  (begin (set-cdr! pair value) #f)
                  "Multiple definition")))
        ((null? r)
         (really-global-define! name value)
         #f)
        (else "Tried to bind in a non-environment")))

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
