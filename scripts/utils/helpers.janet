(defmacro dbg
  "Print expr and its value, then return the value."
  [expr]
  ~(let [res ,expr]
     (print "DEBUG: " (string/format "%q => %q" ',expr res))
     res))

(defn current-date-timestamp
  "Get current date & timestamp string."
  []
  (let [{:year y :month mon :month-day d :hours h :minutes min :seconds s} (os/date (os/time) true)]
    (string/format "%d-%02d-%02dT%02d:%02d:%02d" y (inc mon) (inc d) h min s)))

# TODO: Deprecate and instead is the json output from gh.
(defn print-table
  "Print rows as left-aligned columns under headers."
  [headers rows]
  (def all-rows [headers ;rows])
  (def widths (seq [column :range [0 (length headers)]]
                (max ;(map |(length ($ column)) all-rows))))
  (each row all-rows
    (print (string/trimr (string/join
                           (map |(string $0 (string/repeat " " (- $1 (length $0)))) row widths)
                           "  ")))))
