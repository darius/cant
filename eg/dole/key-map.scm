;; Binding keys to commands

(to (ctrl ch)                           ;XXX duplicate code
  (let code ch.uppercase.code)
  (char<- (- code 64)))

(to (meta ch)
  (string<- esc ch))

(let esc (char<- 27))

(to (key-map<- default-command)
  (let bindings (map<-))
  (make key-map
    ({.set! key command}
     (bindings .set! key command))
    (`(,key)
     (if (eof? key)
         'exit
         (bindings .get key default-command)))
    ))

(export
  ctrl meta
  key-map<-)
