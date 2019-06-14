(import (use "eg/squirm/squirm-terp")
  run-file)

(let test-names '("test/smoke-test.scm"
                  "eg/hello.scm"
                  "eg/universal_server.scm"
                  "test/mod-test.scm"
                  "test/throw-test.scm"
                  "test/monitor-test.scm"
                  "test/partner-test.scm"
                  "test/error-test.scm"
                  "test/recur-test.scm"
                  "test/timeout-test.scm"
                  "test/quasiquote-test.scm"
                  ))

(for each! ((test test-names))
  (format "\nTesting ~d\n" test)
  (run-file (chain "eg/squirm/" test)))
