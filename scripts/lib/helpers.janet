(defn values-from-keys
  "Extract each KEY's value from X"
  [x keys]
  (map |(get x $) keys))

(defmacro dbg
  "Apply `dbg` to expr when env var DBG=true, else return its value."
  [expr]
  ~(if (= (os/getenv "DBG") "true")
     (let [res ,expr]
       (print "DBG: " (string/format "%q\n=> %q" ',expr res))
       res)
     ,expr))

(defn current-date-timestamp
  "Get current date & timestamp string."
  []
  (let [{:year y :month mon :month-day d :hours h :minutes min :seconds s} (os/date (os/time) true)]
    (string/format "%d-%02d-%02dT%02d:%02d:%02d" y (inc mon) (inc d) h min s)))
