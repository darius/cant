(library (player setting)
(export setting? make-setting setting-a-list
        setting-binds? setting-extend-promises
        mutable-setting?
        setting/missing
        setting-lookup global-lookup
        setting-extend-promises setting-resolve!
        setting-extend
        setting-inner-variables
        )
(import (chezscheme) (player thing) (player env))

;; Wrapper for settings
;; The representation will change soon

(define-record-type setting (fields a-list))

(define setting/missing (list '*missing*))

(define (setting-extend variables values setting)
  (make-setting
   (append (map cons variables values)
           (setting-a-list setting))))

(define (setting-lookup setting variable)
  (let ((r (setting-a-list setting)))
    (cond ((assq variable r) => cdr)
          (else (global-lookup variable)))))

(define (global-lookup v)
  (let ((value (really-global-lookup v)))
    (if (eq? value missing)
        setting/missing
        value)))

;; TODO skip if vs null
(define (setting-extend-promises setting vs)
  (let consing ((vs vs)
                (r (setting-a-list setting)))
;;  (let consing ((vs vs) (r r))
    (if (null? vs)
        (make-setting r)
        (consing (cdr vs) (cons (cons (car vs) uninitialized) r)))))

;; Return #f on success, else a complaint.
(define (setting-resolve! setting name value)
  (let ((r (setting-a-list setting)))
    (cond ((assq name r)
           => (lambda (pair)
                (if (eq? (cdr pair) uninitialized)
                    (begin (set-cdr! pair value) #f)
                    "Multiple definition")))
          ((null? r)
           (really-global-define! name value)
           #f)
          (else "Tried to bind in a non-environment"))))

(define (mutable-setting? setting)
  (null? (setting-a-list setting)))

(define (setting-binds? setting variable)
  (or (assq variable (setting-a-list setting))
      (global-defined? variable)))

(define (setting-inner-variables setting)
  ;; TODO dedupe
  (map car (setting-a-list setting)))

)
