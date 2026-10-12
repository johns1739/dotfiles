(defn current-date-timestamp
  "Get current date & timestamp string."
  []
  (let [{:year y :month mon :month-day d :hours h :minutes min :seconds s} (os/date (os/time))]
    (string/format "%d-%02d-%02dT%02d:%02d:%02d" y (inc mon) (inc d) h min s)))
