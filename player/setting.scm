(library (player setting)
(export setting? make-setting setting-a-list
        setting-binds?
        )
(import (chezscheme) (player util) (player env))

;; Wrapper for settings
;; The representation will change soon

(define-record-type setting (fields a-list))

(define (setting-binds? setting variable)
  (or (assq variable (setting-a-list setting))
      (global-defined? variable)))

)
