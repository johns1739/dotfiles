(use sh)
(import spork/json)

(def issue-fields "number,title,url,createdAt,labels,subIssuesSummary,blockedBy")

(defn assigned-issues
  "List open issues in repo (owner/name) assigned to me."
  [repo]
  (json/decode
    ($< gh issue list -R ,repo --assignee "@me" --limit 1000 --json ,issue-fields)
    true true))
