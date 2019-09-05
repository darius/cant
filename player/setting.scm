(library (player setting)
(export setting? empty-setting setting-extend-mutable
        setting-binds? setting-extend-promises
        mutable-setting?
        setting/missing
        setting-lookup
        setting-extend-promises setting-resolve!
        setting-extend
        setting-ensure-bound
        setting-inner-variables
        )
(import (chezscheme) (player util) (player thing))

(define-record-type setting (fields table values parent))
;; parent: #f or another setting
;; Two variants:
;;   - interactive:
;;       table: an eq-hashtable
;;       values: #f
;;   - non:
;;       table: a list of symbols
;;       values: a vector, of the same length

(define empty-setting (make-setting '() (vector) #f))

(define (setting-extend-mutable setting)
  (make-setting (make-eq-hashtable) #f setting))

(define (mutable-setting? x)
  (and (setting? x)
       (eq-hashtable? (setting-table x))))

(define setting/missing (list '*missing*))

(define (setting-extend variables values setting)
  (make-setting variables values setting))

(define (setting-extend-promises setting variables)
  (if (null? variables)
      setting
      (make-setting variables
                    (make-vector (length variables) uninitialized)
                    setting)))

;; Return #f on success, else a complaint.
(define (setting-resolve! setting variable value)
  (let ((table (setting-table setting)))
    (cond ((and (pair? table) (frame-index table variable))
           => (lambda (i)
                (let ((values (setting-values setting)))
                  (cond ((eq? (vector-ref values i) uninitialized)
                         (vector-set! values i value)
                         #f)
                        (else "Multiple definition")))))
          ((eq-hashtable? table)
           (if (setting-binds? (setting-parent setting) variable)
               "An interactive setting may not shadow its parent" 
               (begin
                 (unless (eq? (eq-hashtable-ref table variable uninitialized)
                              uninitialized)
                   (display "\nWarning: redefined ") (write variable) (newline))
                 (eq-hashtable-set! table variable value)
                 #f)))
          (else
           "Somehow trying to bind a variable in the wrong frame"))))

(define (setting-binds? setting variable)
  (let walking ((setting setting))
    (and setting
         (let ((table (setting-table setting)))
           (or (if (eq-hashtable? table)
                   (eq-hashtable-contains? table variable) ;TODO what about uninitialized?
                   (memq variable table))
               (walking (setting-parent setting)))))))

(define (setting-lookup setting variable)
  (define (walking setting)
    (let ((table (setting-table setting)))
      (if (eq-hashtable? table)
          (let ((v (eq-hashtable-ref table variable setting/missing)))
            (if (eq? v setting/missing)
                (keep-walking setting)
                v))
          (cond ((frame-index table variable)
                 => (lambda (n) (vector-ref (setting-values setting) n)))
                (else (keep-walking setting))))))
  (define (keep-walking setting)
    (cond ((setting-parent setting) => walking)
          (else setting/missing)))
  (walking setting))

(define (setting-ensure-bound setting variables)
  (cond ((null? variables) setting)
        ((mutable-setting? setting)
         (let ((table (setting-table setting)))
           (for-each (lambda (v)
                       (insist (eq? (eq-hashtable-ref table v uninitialized)
                                    uninitialized)
                               "Already bound" v)
                       (eq-hashtable-set! table v uninitialized))
                     variables))
         setting)
        (else
         (setting-extend-promises setting variables))))

(define (setting-inner-variables setting)
  (let walking ((setting setting))
    (if (not (setting-parent setting))
        '()
        ;; TODO dedupe
        (append (let ((table (setting-table setting)))
                  (if (eq-hashtable? table)
                      (vector->list (hashtable-keys table))
                      table))
                (walking (setting-parent setting))))))

(define (frame-index vars v)
  (let scanning ((i 0) (vars vars))
    (cond ((null? vars) #f)
          ((eq? (car vars) v) i)
          (else (scanning (+ i 1) (cdr vars))))))

)
