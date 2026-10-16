(use judge)
(array/push module/paths [":cur:/:all:" :source |(string/has-prefix? "." $)])
(import ./open_issues)

(def issues
  (trust (open_issues/repo-issues "owner/repo")
    @[@{:blockedBy @{:nodes @[@{:state "OPEN"} @{:state "CLOSED"} @{:state "OPEN"}]}
        :createdAt "2026-01-01T00:00:00Z"
        :labels @[@{:name "bug"} @{:name "api"}]
        :number 12
        :subIssuesSummary @{:total 3}
        :title "Fix login"
        :url "https://github.com/owner/repo/issues/12"}]))

(def issue (first issues))

(test (open_issues/issue-reference issue) "repo#12")

(test (open_issues/count-open-blockers issue) "2")

(test (open_issues/issue-row issue)
  ["bug, api" "Fix login" "https://github.com/owner/repo/issues/12" "3" "2"])
