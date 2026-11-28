(import ./gh)
(use ./helpers)

(defn format-datetime
  [datetime]
  (string/slice datetime 0 10))

(defn table-print-prs
  [prs]
  (def format "%-12.12s %-12.12s %-50.50s %-20.20s %5.5s %5.5s")
  (printf format "Repo" "Author" "Head Ref" "Outcome" "files" "lines")
  (each pr prs
    (printf format
            (gh/get-in-search-prs pr [:repository :name] "NONE")
            (get-in pr [:author :login])
            (pr :headRefName)
            (pr :reviewDecision)
            (string (pr :changedFiles))
            (string (pr :additions)))))

(defn table-print-issues
  [issues]
  (def format "%-12.12s %-60.60s %9.9s %9.9s %9.9s %s %s")
  (printf format "Repo" "Title" "Blocked" "Blocking" "SubIssues" "CreatedAt" "UpdatedAt")
  (each issue issues
    (printf format
            (gh/get-in-search-issues issue [:repository :name] "NONE")
            (issue :title)
            (string (get-in issue [:blockedBy :totalCount]))
            (string (get-in issue [:blocking :totalCount]))
            (string (get-in issue [:subIssues :totalCount]))
            (format-datetime (issue :createdAt))
            (format-datetime (issue :updatedAt)))))

(defn collect-and-table-print-prs
  "Report all my open prs"
  []
  (let [search-prs (gh/search-prs)
        owner-repos (gh/extract-owner-repos search-prs)
        prs (gh/pr-list owner-repos)
        sorted-prs (sorted-by (fn [i] (i :createdAt)) prs)]
    (table-print-prs sorted-prs)))

(defn collect-and-table-print-issues
  "Report all my open issues"
  []
  (let [search-issues (gh/search-issues)
        owner-repos (gh/extract-owner-repos search-issues)
        issues (gh/issue-list owner-repos)
        sorted-issues (sorted-by (fn [i] (i :createdAt)) issues)]
    (table-print-issues sorted-issues)))
