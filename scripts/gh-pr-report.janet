#!/usr/bin/env janet

(import ./lib/gh)

(defn print-table-pull-requests
  [prs]
  (def format "%-12.12s %-50.50s %-20.20s %5.5s %5.5s")
  (printf format "Repo" "Head Ref" "Outcome" "files" "lines")
  (printf format (string/repeat "-" 12) (string/repeat "-" 50) (string/repeat "-" 20) "-----" "-----")
  (each pr prs
    (printf format
            (gh/get-in-cache pr gh/gh-search-entry-by-id [:repository :name])
            (pr :headRefName)
            (pr :reviewDecision)
            (string (pr :changedFiles))
            (string (pr :additions)))))

(defn gh-pr-report
  "Print pull-request status report for ORGS"
  []
  (let [search-prs (gh/gh-search-prs)
        owner-repos (gh/extract-owner-repos search-prs)
        prs (gh/gh-pr-list owner-repos)]
    (print-table-pull-requests (sorted-by (fn [i] (i :createdAt)) prs))))

(defn main
  [_script]
  (gh-pr-report))
