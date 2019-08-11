;; Using modules and loading source files

(let the-modules (map<-))

;; To make it possible to reload a module by calling (use file-stem)
;; again afterward. N.B. that won't mutate the existing module object.
(to (unuse file-stem)                   ;TODO better name
  (the-modules .delete! file-stem))

(to (use file-stem)                  ;TODO a realer module system
  ;; N.B. could sort of just use memoize if that were already loaded.
  (let stem (if (symbol? file-stem)
                (chain "library/" file-stem.name)
                file-stem))
  (may (the-modules .get stem)
    (be #no
      (let mod (load-module (chain stem ".scm") `(,stem)))
      (the-modules .set! stem mod)
      mod)
    (be mod
      mod)))

(to (load filename @(optional context))
  (load-exp `(do ,@(with-input-file read-all filename))
            context))

(to (load-module filename @(optional context))
  (load-exp `(hide ,@(with-input-file read-all filename))
            context))

(to (load-exp exp context)
  (let code (parse-exp exp (or context '())))
  (evaluate code '()))
