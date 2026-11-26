(defn values-from-keys
  "Extract each KEY's value from X"
  [x keys]
  (map |(get x $) keys))

(defmacro dbg
  "Print expr and its value, then return the value."
  [expr]
  ~(let [res ,expr]
     (print "DEBUG: " (string/format "%q => %q" ',expr res))
     res))

(defmacro dbg-if-debug
  "Apply `dbg` to expr when env var DEBUG=true, else return its value."
  [expr]
  ~(if (= (os/getenv "DEBUG") "true")
     (dbg ,expr)
     ,expr))

(defn current-date-timestamp
  "Get current date & timestamp string."
  []
  (let [{:year y :month mon :month-day d :hours h :minutes min :seconds s} (os/date (os/time) true)]
    (string/format "%d-%02d-%02dT%02d:%02d:%02d" y (inc mon) (inc d) h min s)))
