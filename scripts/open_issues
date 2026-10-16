#!/usr/bin/env janet

(import ./utils/gh)
(use ./utils/helpers)

(def headers ["LABELS" "TITLE" "URL" "SUB-ISSUES" "BLOCKED COUNT"])

(defn issue-reference
  "Short issue reference, e.g. repo#123."
  [issue]
  (let [url (issue :url)
        [repo-name number] (peg/match
                             '(* "https://github.com/"
                                 (thru "/")
                                 (<- (to "/"))
                                 "/issues/"
                                 (<- :d+))
                             url)]
    (string repo-name "#" number)))

(defn count-open-blockers
  "Number of open issues blocking issue, as a string."
  [issue]
  (string (->> (get-in issue [:blockedBy :nodes])
               (filter |(= "OPEN" ($ :state)))
               (length))))

(defn issue-row
  "Table row for issue."
  [issue]
  [(string/join (map |($ :name) (issue :labels)) ", ")
   (issue :title)
   (issue :url)
   (string (get-in issue [:subIssuesSummary :total]))
   (count-open-blockers issue)])

(defn repo-issues
  "Issues assigned to me in repo, or empty on failure."
  [repo]
  (try
    (gh/assigned-issues repo)
    ([_error]
      (eprint "FAILED " repo)
      @[])))

(defn main
  "Print a table of issues assigned to me in the given repos."
  [_script & repos]
  (->> repos
       (mapcat repo-issues)
       (sort-by |($ :createdAt))
       (map issue-row)
       (print-table headers)))
