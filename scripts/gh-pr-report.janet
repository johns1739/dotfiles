#!/usr/bin/env janet

(use spork/sh-dsl)
(import spork/json)
(import ./lib/gh)

# JSON FIELDS
#   additions, assignees, author, autoMergeRequest, baseRefName, baseRefOid, body,
#   changedFiles, closed, closedAt, closingIssuesReferences, comments, commits,
#   createdAt, deletions, files, fullDatabaseId, headRefName, headRefOid,
#   headRepository, headRepositoryOwner, id, isCrossRepository, isDraft, labels,
#   latestReviews, maintainerCanModify, mergeCommit, mergeStateStatus, mergeable,
#   mergedAt, mergedBy, milestone, number, potentialMergeCommit, projectCards,
#   projectItems, reactionGroups, reviewDecision, reviewRequests, reviews, state,
#   statusCheckRollup, title, updatedAt, url

(defn values-from-keys
  "Extract each KEY's value from X"
  [x keys]
  (map |(get x $) keys))

(def fields
  [:baseRefName
   :headRefName
   :title
   :createdAt
   :additions
   :changedFiles
   :reviewDecision])

(defn print-table-pull-requests
  [prs]
  (def format "%-12s %-50.50s %-20.20s %5.5s %5.5s")
  (printf format "Repo" "Head Ref" "Outcome" "files" "lines")
  (printf format (string/repeat "-" 12) (string/repeat "-" 50) (string/repeat "-" 20) "-----" "-----")
  (each pr prs
    (printf format
            (pr :repoName)
            (pr :headRefName)
            (pr :reviewDecision)
            (string (pr :changedFiles))
            (string (pr :additions)))))

(defn gh-pr-report
  "Print pull-request status report for ORGS"
  [repos]
  (def prs
    (mapcat
      (fn [repo]
        (def items (-> ($< gh pr list
                           --author "@me"
                           -R ,repo
                           --json ,(string/join fields ",")
                           --state open
                           --limit 20)
                       (json/decode :use-keywords :use-nils)))
        (def repo-short (last (string/split "/" repo)))
        (map |(put $ :repoName repo-short) items))
      repos))
  (print-table-pull-requests (sorted-by (fn [i] (i :createdAt)) prs)))

(defn main
  [_script & repos]
  (gh-pr-report repos))
