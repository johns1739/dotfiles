(use judge)
(import ./gh)

(trust (gh/assigned-issues "owner/repo")
  @[@{:blockedBy @{:nodes @[] :totalCount 0}
      :createdAt "2026-01-01T00:00:00Z"
      :labels @[@{:name "bug"}]
      :number 1
      :subIssuesSummary @{:completed 0 :percentCompleted 0 :total 0}
      :title "Issue"
      :url "https://github.com/owner/repo/issues/1"}])
