(use judge)
(use ./helpers)

(test-stdout (dbg (+ 1 2)) `
  DEBUG: (+ 1 2) => 3
` 3)

(test-stdout (dbg (tuple 1 2)) `
  DEBUG: (tuple 1 2) => (1 2)
` (1 2))

(deftest "dbg-if-debug"
  (os/setenv "DEBUG" nil)
  (def output @"")
  (test (with-dyns [*out* output] (dbg-if-debug (+ 1 2))) 3)
  (test output @"")
  (os/setenv "DEBUG" "true")
  (test-stdout (dbg-if-debug (+ 1 2)) `
    DEBUG: (+ 1 2) => 3
  ` 3)
  (os/setenv "DEBUG" nil))

(test (peg/match ~(* (4 :d) "-" (2 :d) "-" (2 :d) "T" (2 :d) ":" (2 :d) ":" (2 :d) -1)
                 (current-date-timestamp))
  @[])

(test-stdout (print-table ["NAME" "COUNT"] [["alpha" "1"] ["b" "22"]]) `
  NAME   COUNT
  alpha  1
  b      22
`)
