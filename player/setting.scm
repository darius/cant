;; A setting is Cant's jargon for an environment.

(library (player setting)
(export setting? empty-setting make-setting setting-extend-mutable
        setting-binds? setting-extend-promises
        mutable-setting?
        setting/missing
        setting-address setting-lookup setting-address-fetch
        setting-extend-promises setting-resolve! setting-address-resolve!
        setting-extend
        setting-ensure-bound
        setting-parent setting-inner-variables
        )
(import (chezscheme) (player util) (player thing))

(define-record-type setting (fields table (mutable values) parent))
;; parent: #f or another setting
;; values: a vector, of length = the number of variables in the table
;; Two variants:
;;   - interactive:
;;       table: an eq-hashtable, symbol -> index into values
;;   - non:
;;       table: a list of symbols

;; TODO values is a bad name since it's a Scheme primitive these days

(define empty-setting (make-setting '() (vector) #f))

(define (setting-extend-mutable setting)
  (make-setting (make-eq-hashtable) (vector) setting))

(define (mutable-setting? x)
  (and (setting? x)
       (eq-hashtable? (setting-table x))))

(define setting/missing (list '*missing*))

(define (setting-extend variables values setting)
  (insist (and (vector? values)
               (= (length variables) (vector-length values)))
          "values must be a vector matching variables" values)
  (make-setting variables values setting))

(define (setting-extend-promises setting variables)
  (if (null? variables)
      setting
      (make-setting variables
                    (make-vector (length variables) uninitialized)
                    setting)))

;; Return #f on success, else a complaint.
(define (setting-resolve! setting variable value)
  (cond ((setting-address setting variable)
         => (lambda (pair)
              (let ((depth (car pair)) (offset (cadr pair)))
                (setting-address-resolve! setting depth offset variable value))))
        (else 
         "Somehow trying to bind a variable in the wrong frame")))

(define (setting-address-resolve! setting depth offset variable value)
  (let walking ((s setting) (d depth))
    (cond ((= d 0)
           (let* ((values (setting-values s))
                  (redef? (not (eq? (vector-ref values offset) uninitialized))))
             (if (and redef? (not (eq-hashtable? (setting-table s))))
                 "Multiple definition"
                 (begin
                   (when (and redef?
                              (not (eq? variable '$$))) ; Hack: the listener's $$ gets redefined all the time, too noisy.
                     (display "\nWarning: redefined ") (write variable) (newline))
                   (vector-set! values offset value)
                   #f))))
          (else
           (walking (setting-parent s) (- d 1))))))

(define (setting-binds? setting variable)
  (let walking ((setting setting))
    (and setting
         (let ((table (setting-table setting)))
           (or (if (eq-hashtable? table)
                   (eq-hashtable-contains? table variable) ;TODO what about uninitialized?
                   (memq variable table))
               (walking (setting-parent setting)))))))

(define (setting-address setting variable)
  (let walking ((depth 0) (setting setting))
    (cond ((let ((table (setting-table setting)))
             (if (eq-hashtable? table)
                 (eq-hashtable-ref table variable #f)
                 (frame-index table variable)))
           => (lambda (index)
                (list depth index)))
          ((setting-parent setting)
           => (lambda (parent)
                (walking (+ depth 1) parent)))
          (else #f))))

(define (setting-address-fetch setting depth offset)
  (let walking ((s setting) (d depth))
    (if (= d 0)
        (vector-ref (setting-values s) offset)
        (walking (setting-parent s) (- d 1)))))

(define (setting-lookup setting variable)
  (cond ((setting-address setting variable)
         => (lambda (pair)
              (let ((depth (car pair)) (offset (cadr pair)))
                (setting-address-fetch setting depth offset))))
        (else setting/missing)))

;; Return a setting with bindings for `variables`.
;; If `setting` is mutable, then just add to that setting.
;; If `setting` is immutable, then return an extension of it.
(define (setting-ensure-bound setting variables)
  (cond ((mutable-setting? setting)
         (let ((table (setting-table setting)))
           (for-each (lambda (v)
                       ;; TODO un-quadratify
                       (cond ((eq-hashtable-ref table v #f))
;; XXX Why did I have this insist? I'm disabling it now to allow interactive redefinition.
;; Was that supposed to work some other way?
;;                              => (lambda (i)
;;                                   (insist (eq? (vector-ref (setting-values setting) i)
;;                                                uninitialized)
;;                                           "Already bound" v)))
                             ((setting-binds? (setting-parent setting) v)
                              (error 'setting-ensure-bound
                                     "An interactive setting may not shadow its parent"
                                     v))
                             (else
                              (let* ((old-values (setting-values setting))
                                     (n (vector-length old-values))
                                     (new-values (make-vector (+ n 1))))
                                (do ((i 0 (+ i 1)))
                                    ((= i n))
                                  (vector-set! new-values i (vector-ref old-values i)))
                                (vector-set! new-values n uninitialized)
                                (setting-values-set! setting new-values)
                                (eq-hashtable-set! table v n)))))
                     variables))
         setting)
        (else
         (setting-extend-promises setting variables))))

(define (setting-inner-variables setting)
  (let ((table (setting-table setting)))
    (if (eq-hashtable? table)
        (vector->list (hashtable-keys table))
        table)))

(define (frame-index vars v)
  (let scanning ((i 0) (vars vars))
    (cond ((null? vars) #f)
          ((eq? (car vars) v) i)
          (else (scanning (+ i 1) (cdr vars))))))

)
